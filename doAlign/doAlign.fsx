//CELL 1

let transcriptFilePath = "/y/south-park-1-to-20/SouthParkTranscripts/s1-20.tsv"

type TranscriptLine = 
    {
        Season: int
        Episode: int
        Character: string
        Line: string
    }
    override x.ToString() = x.Line

//Parse into TranscriptLines
let transcriptLines = 
    transcriptFilePath 
    |> System.IO.File.ReadAllLines
    |> Seq.skip 1 //header
    |> Seq.map(fun line ->
                  let s = line.Split('\t')
                  let season = System.Int32.Parse(s.[0])
                  let episode =
                        match season,System.Int32.Parse(s.[1]) with
                        //"Volcano" comes second on DVD but otherwise seems to be 3rd episode
                        | 1,2 -> 3
                        | 1,3 -> 2
                        //Jakovasaurs and Tweak vs Craig are switched in transcripts
                        | 3,4 -> 5 
                        | 3,5 -> 4 
                        //switched in transcripts
                        | 3,10 -> 11 
                        | 3,12 -> 10
                        | 3,13 -> 12
                        | 3,11 -> 13
                        //switched in transcripts
                        | 4,2 -> 1
                        | 4,1 -> 2
                        | 4,3 -> 4
                        | 4,4 -> 3
                        | 4,5 -> 14
                        | 4,6 -> 5
                        | 4,7 -> 6
                        | 4,8 -> 7
                        | 4,9 -> 8
                        | 4,10 -> 9
                        | 4,11 -> 10
                        | 4,12 -> 11
                        | 4,13 -> 12
                        | 4,14 -> 13
                        //switched in transcripts
                        //| 6,4 -> 5
                        //| 6,5 -> 4
                        | _,j -> j
                  {Season = season; Episode = episode; Character = s.[2]; Line = s.[3].Trim('"')}
                )

//CELL 2

#r "/z/aolney/repos/Newtonsoft.Json.9.0.1/lib/net40/Newtonsoft.Json.dll"

//Chapter constructs are unused as explained in the notes, but could represent the most useful way to do episode segmentation in other contexts, so leaving for future reference
type Chapter =
    {
        id : int
        time_base: string
        start: int
        start_time: float
        ``end``: int
        end_time: float
        tags: Map<string,string>
    }
type DiscChapters =
    {
        chapters : Chapter[]
    }

//We intentionally ignore words and phonemes that are part of this b/c we don't need them
type Subtitle =
    {
        subtitle : string
        edited_text : string
        start : int
        ``end`` : int
    }
    override x.ToString() = x.edited_text + "-" + x.start.ToString()  + ":" + x.``end``.ToString()

type CCAligned =
    {
        subtitles : Subtitle[]
    }
    
///The CCAligned json is invalid; we must fix unescaped quotes and put commas b/w objects
let FixCCAlignedJSON filePath =
    let json = 
        filePath
        |> System.IO.File.ReadAllLines
        |> Seq.map( fun line -> line.Replace("} {","},  {"))
        |> Seq.map( fun line -> line.Replace("}\t{","},  {")) //new version of json.net requires
        |> Seq.map( fun line -> line.Replace("\\","")) //bad escape sequences - bad OCR?
        |> Seq.map( fun line ->
                   let fieldIndex = line.IndexOf(":")
                   if fieldIndex > 0 then 
                       let propertyString = line.Substring(fieldIndex+1)
                       if propertyString.Contains("\"") then
                           line.Substring(0,fieldIndex + 1) + "\"" + propertyString.Trim([|' ';'"';','|]).Replace("\"","\\\"") + "\","
                       else
                           line
                   else 
                       line
                  )
    System.IO.File.WriteAllLines( filePath + ".corrected",json )
    let correctedJson = System.IO.File.ReadAllText( filePath + ".corrected" )
    let ccAligned = Newtonsoft.Json.JsonConvert.DeserializeObject<CCAligned>(correctedJson)
    //
    ccAligned

type Alignment =
    {
        Season: int
	Disc: int
        Episode: int
        Character: string
        Turn: string //can contain multiple utterances
        TurnIndex : int
        Subtitles : Subtitle[]
    }
    override x.ToString() = x.Turn

type TTSData =
    {
        Start : int
        Stop : int
        Text : string
        Id : string
    }
    
//http://www.fssnip.net/bc/title/EditDistance
//modified for memory reasons
type DistanceType = MinimumEditDistance | LevenshteinDistance

let getEditDistance distanceType (X:string) (Y:string) =
    let m = X.Length
    let n = Y.Length
    let d = Array2D.init (m + 1) (n + 1) (fun i j -> if j = 0 then i elif i = 0 then j else 0)
    let ptr = //System.Collections.Generic.Dictionary<string,char>()// 
        Array2D.init (m + 1) (n + 1) (fun i j -> if j = 0 then 'd' elif i = 0 then 'i' else 's')
    //let ptr = Array2D.init (m + 1) (n + 1) (fun i j -> if j = 0 then Deletion elif i = 0 then Insertion else Substitution)
    let penalizationForSubstitution = 
        match distanceType with
        | MinimumEditDistance -> 1
        | LevenshteinDistance -> 2
    for i in 1..m do
        for j in 1..n do
            let a, b = Seq.minBy fst [d.[i-1, j] + 1, 'd' //Deletion
                                      d.[i, j-1] + 1, 'i' //Insertion
                                      d.[i-1, j-1] + (if X.[i-1] <> Y.[j-1] then penalizationForSubstitution else 0), 's'] //Substitution]
            d.[i, j] <- a
            ptr.[i, j] <- b
    let alignment = 
        (m, n) 
        |> Seq.unfold (fun (i, j) -> 
            if i = 0 && j = 0 then
                None
            else
                match ptr.[i, j] with
                | 'd' -> Some((X.[i-1], '*'), (i-1, j))
                | 'i' -> Some(('*', Y.[j-1]), (i, j-1))
                | 's' -> Some((X.[i-1], Y.[j-1]), (i-1, j-1)) //)
                | _ -> None )
        |> Array.ofSeq
        |> Array.rev
    d.[m, n], alignment

let printAlignment alignment season episode directory = 
    let toString (chars : char array) = new string(chars)
    //System.IO.File.WriteAllText( directory + "transcript" + season.ToString() + "-" + episode.ToString() + ".txt", alignment |> Array.map fst |> toString ) //|> printfn "%s"
    //System.IO.File.WriteAllText( directory + "subtitles"  + season.ToString() + "-" + episode.ToString() + ".txt", alignment |> Array.map snd |> toString ) //|> printfn "%s"
    System.IO.File.WriteAllText( directory + season.ToString() + "-" + episode.ToString() + ".editalignment", (alignment |> Array.map fst |> toString) + "\n" + (alignment |> Array.map snd |> toString) ) 

///Return a sequence of episode subtitle sequences that approximately match transcripts (no theme song, no DVD special introduction)
let GetCleanEpisodeSubtitles ( subtitleLines : Subtitle[] ) subtitleSeason = //(transcriptNormLines : seq<TranscriptLine>) 

    //First line of song has inconsistent subtitles; 
    //using the second line GONNA HAVE MYSELF A TIME; 
    //"Monkey Phonics" missing that so changed to "FRIENDLY FACES EVERYWHERE"
    //s6-e1 missing so changed to "HUMBLE FOLKS WITHOUT TEMPTATION"
    //S1-E9 has no song, using THE CHRISTMAS CHOCOLATE BAR
    //S2-E1 has no song, using WHAT A BRILLIANT PIECE OF WORK IT IS
    //S3-E13 has no song, using THIS IS MARKLAR
    //S3-E15 has no song, using ALL HEARD OF RUDOLPH AND HIS SHINY NOSE
    //S4-E10 has no song, using TODAY WE ARE GOING TO TALK ABOUT HELL //everyone in this town ourselves
    //s4-e14 has no song, using AHH DICKENS / THE IMAGERY OF COBBLESTONE STREETS
    //S6-E2 has no song, using THANKS FOR HAVING US ALL OVER FOR DINNER CHRIS AND LINDA
    //s6-e4 has no MEET SOME FRIENDS OF MINE changed to SO COME ON DOWN TO SOUTH PARK
    //17-E8 has no song OF THE MEN LADY MCKORMICK / YOU CANT DIEL
    //17-e9 has alt song Yes | think that will do nicely / f Flopping wieners flopping wieners I
    //let songStartRegex = new System.Text.RegularExpressions.Regex( "THANKS FOR HAVING US ALL OVER FOR DINNER CHRIS AND LINDA|HUMBLE FOLKS WITHOUT TEMPTATION|AHH DICKENS|TODAY WE ARE GOING TO TALK ABOUT HELL|ALL HEARD OF RUDOLPH AND HIS SHINY NOSE|THIS IS MARKLAR|THE CHRISTMAS CHOCOLATE BAR|WHAT A BRILLIANT PIECE OF WORK IT IS", System.Text.RegularExpressions.RegexOptions.IgnoreCase )
    //let songEndRegex = new System.Text.RegularExpressions.Regex( "SO COME ON DOWN TO SOUTH PARK|YES IT WAS FABULOUS|THE IMAGERY OF COBBLESTONE STREETS|EVERYONE IN THIS TOWN OURSELVES|ALL HEARD OF RUDOLPH AND HIS SHINY NOSE|THIS IS MARKLAR|THE CHRISTMAS CHOCOLATE BAR|WHAT A BRILLIANT PIECE OF WORK IT IS", System.Text.RegularExpressions.RegexOptions.IgnoreCase )
    let songStartRegex = new System.Text.RegularExpressions.Regex( "GO[^D]+DOWN TO SOUTH PARK|HAVE MYSELF A TIME|FRIENDLY FACES EVERYWHERE|HUMBLE FOLKS WITHOUT TEMPTATION|DEATH AND SADNESS EVERYWHERE|LONELINESS AND DEGRADATION", System.Text.RegularExpressions.RegexOptions.IgnoreCase )
    let altStartRegex = new System.Text.RegularExpressions.Regex( "THINK THAT WILL DO NICELY|OF THE MEN LADY MCKORMICK|THANKS FOR HAVING US ALL OVER FOR DINNER CHRIS AND LINDA|AHH DICKENS|TODAY WE ARE GOING TO TALK ABOUT HELL|ALL HEARD OF RUDOLPH AND HIS SHINY NOSE|THIS IS MARKLAR|THE CHRISTMAS CHOCOLATE BAR|WHAT A BRILLIANT PIECE OF WORK IT IS", System.Text.RegularExpressions.RegexOptions.IgnoreCase )
    let songEndRegex = new System.Text.RegularExpressions.Regex( "LEAVE MY WOES BEHIND|AMPLE PARKING DAY OR NIGHT|SPOUTING HOWDY NEIGHBOR|SEE IF I CAN'T UNWIND|COME ON DOWN TO SOUTH PARK|MEET SOME FRIENDS OF MINE|TWO POSERS SPOUTING LET'S GO SHOPPING|TO DIE TO DIE", System.Text.RegularExpressions.RegexOptions.IgnoreCase )
    let altEndRegex = new System.Text.RegularExpressions.Regex( "J NONERECT WIENERS JF|YOU CANT DIEL|YES IT WAS FABULOUS|THE IMAGERY OF COBBLESTONE STREETS|EVERYONE IN THIS TOWN OURSELVES|ALL HEARD OF RUDOLPH AND HIS SHINY NOSE|THIS IS MARKLAR|THE CHRISTMAS CHOCOLATE BAR|WHAT A BRILLIANT PIECE OF WORK IT IS", System.Text.RegularExpressions.RegexOptions.IgnoreCase )

    //Get the song start subtitle and index by season/episode
    // PROBLEM: very commonly subtitles are missing for any given part of the song we might select
    // so we look for multiple lines but then only choose one at either the beginning or end (using firstLast function)
    let GetSongBoundarySubtitleMap (songRegex : System.Text.RegularExpressions.Regex ) (altRegex :System.Text.RegularExpressions.Regex) firstLast subLines = 
        seq {
            //first use the song. This implicitly requires multiple song matches
            let threshold = 10000
            let candidateTuples = 
                subLines
                |> Seq.choose( fun s -> if songRegex.IsMatch( s.edited_text )  then Some(s) else None )
                |> Seq.pairwise
                |> Seq.map( fun (s1,s2) -> if s2.start - s1.``end`` < threshold then (s1,true) else (s2,false)  ) //calculate gap, make sure its < 10 sec
                //|> Seq.sortBy( fun (s,bool) -> (s.start,bool|>not) )
                |> ResizeArray
            let debugCopy = candidateTuples.ToArray()
            //for unknown reasons, sometimes the first element is false
            if candidateTuples.[0] |> snd = false then
                candidateTuples.RemoveAt(0)
            while candidateTuples.Count > 0 do
                let song = candidateTuples |> Seq.toArray |> Array.takeWhile snd //true means close
                if song.Length = 0 then 
                    failwith "Possible missing song"
                let debugYield = song |> firstLast |> fst 
                yield debugYield
                candidateTuples.RemoveRange(0, song.Length )
                let gap = candidateTuples |> Seq.toArray |> Array.takeWhile( snd >> not )  //false means a gap
                candidateTuples.RemoveRange(0, gap.Length )
            //now use alternative matches. Only one match is required
            for alt in subLines |> Seq.choose( fun s -> if altRegex.IsMatch( s.edited_text )  then Some(s) else None ) do
                yield alt
        }
        |> Seq.sortBy( fun s -> s.start ) //sorting needed b/c our alt matches came in a second traversal
        |> Seq.mapi( fun i s -> (subtitleSeason, i + 1), s )
        |> Map.ofSeq

    let songStartSubtitleMap = subtitleLines |> GetSongBoundarySubtitleMap songStartRegex altStartRegex Array.head
    let songEndSubtitleMap = subtitleLines |> GetSongBoundarySubtitleMap songEndRegex altEndRegex Array.last

    //let songStartSubtitleMap =
    //    subtitleLines
    //    |> Seq.choose( fun s -> if songStartRegex.IsMatch( s.edited_text )  then Some(s) else None )
    //    |> Seq.mapi( fun i s -> (subtitleSeason, i + 1), s )
    //    |> Map.ofSeq

        
    //From the possible last subtitles find the subtitle that comes right before the following song
    let lastSubtitleMap =
        songStartSubtitleMap
        |> Seq.choose( fun (KeyValue((season,episode),songStartSubtitle)) -> 
            let preceedingPossibleLastSubtitles =
                //original gap approach
                //possibleLastSubtitles
                //|> Seq.map fst
                //end original gap approach
                subtitleLines
                //remove bogus captioning subtitles
                |> Seq.filter( fun s -> s.edited_text.Contains("CAPTIONED") |> not )
                |> Seq.filter( fun pls -> pls.``end`` < songStartSubtitle.start )
            if preceedingPossibleLastSubtitles |> Seq.isEmpty then
                None
            else
                Some( (season,episode), preceedingPossibleLastSubtitles |> Seq.maxBy( fun subtitle -> subtitle.``end`` ) )
            )
        |> Map.ofSeq
       
    let NotToLastSubtitleBeforeNextSong season episode subtitle =
        match lastSubtitleMap.TryFind(season,episode + 1) with
        | Some(lastSubtitle) -> subtitle.``end`` <= lastSubtitle.``end``  
        | _ -> true //assumes no commetary after final episode on disc

    //yield all subtitles bounded by the end of the song to the possible last subtitle that comes before the beginning of the next song
    seq {
        let mutable episode = 1
        let mutable si = 0
        let mutable notDone = true
        let mutable notSongDone = true
        while notDone do
            //seek to end of song
            while si < subtitleLines.Length && subtitleLines.[si] <> songEndSubtitleMap.[(subtitleSeason,episode)] do //(songEndRegex.IsMatch(subtitleLines.[si].edited_text) |> not) do
            //while si < subtitleLines.Length && (subtitleLines.[si].edited_text.Contains("MEET SOME FRIENDS OF MINE") |> not ) do
                si <- si + 1
            si <- si + 1
            // yield until the last transcript match before the start of the next song
            yield seq {
                while si < subtitleLines.Length && NotToLastSubtitleBeforeNextSong subtitleSeason episode subtitleLines.[si] do
                    yield subtitleLines.[si]
                    si <- si + 1
                si <- si + 1
                //if si < subtitleLines.Length then yield subtitleLines.[si]
            } |> Seq.toArray
            episode <- episode + 1
            if si >= subtitleLines.Length then
                notDone <- false
    } |> Seq.toArray

//-------------------------------------------------------------------------------

let whiteSpaceRegex = System.Text.RegularExpressions.Regex("\s+")
let nonApostrophePunctRegex = System.Text.RegularExpressions.Regex("[^\w\s']")
let RemovePunctuation inputString =
    whiteSpaceRegex.Replace( nonApostrophePunctRegex.Replace( inputString, " "), " " ).Trim()
let tagRegex = System.Text.RegularExpressions.Regex("<.*?>") //only acceptable b/c our subtitle markup is so simplistic it does not require CFG parser
let RemoveTags inputString =
    whiteSpaceRegex.Replace( tagRegex.Replace( inputString, " "), " " ).Trim()

//CELL 3

//LOOP TO DO ALIGNMENT FOR ALL DISCS
let dataDirectory = "/y/south-park-1-to-20/"
let ccAlignedJsonFiles =
    System.IO.Directory.GetFiles(dataDirectory, "*.json") 
    |> Seq.map( fun filePath ->
        let seasonDiscSplit = System.IO.Path.GetFileNameWithoutExtension(filePath).Split('-')
        let season = seasonDiscSplit.[0] |> System.Int32.Parse
        let disc = seasonDiscSplit.[1] |> System.Int32.Parse
        (season,disc),filePath )
    |> Seq.sortBy fst
    // !!!!!!!!!!!!!!!!!!!
    // FOR DEBUG ONLY
    // !!!!!!!!!!!!!!!!!!!
    //|> Seq.filter(fun ((season,_),_) -> season > 11 ) //isolate season rather than starting from first

let editDistances = ResizeArray<string>() //output to Jupyter for error checking
let mutable episodeOffset = 0 //we need to increment episodes across discs
let mutable currentSeason = 0 //we need to reset episode offset at each season

for (season,disc),filePath in ccAlignedJsonFiles do
// parallel doesnt work here b/c of mutable state, but could probably work at season level
//let parallelOptions = new System.Threading.Tasks.ParallelOptions()
//parallelOptions.MaxDegreeOfParallelism <- 2
//System.Threading.Tasks.Parallel.ForEach( ccAlignedJsonFiles, parallelOptions,  fun ((season,_),filePath) ->

    if season <> currentSeason then
        currentSeason <- season
        episodeOffset <- 0

    let sLines =
        let ccAligned = FixCCAlignedJSON filePath
        ccAligned.subtitles

    // !!!!!!!!!!!!!!!!!!!
    //TODO: FOR DEBUG ONLY
    // !!!!!!!!!!!!!!!!!!!
    //if season = 6 && episodeOffset >= 0 then
    //    System.Console.WriteLine() //1-3 good unless later changes broke; 4-10 on no subtitles

    let episodeSubtitles = GetCleanEpisodeSubtitles sLines season

    let alignments = ResizeArray<Alignment>()

    episodeSubtitles
    |> Seq.iteri (fun i episodeSubtitles ->

        let episodeTranscriptLines = 
            transcriptLines
            |> Seq.filter( fun tl -> tl.Season = season && tl.Episode = i + 1 + episodeOffset)
            |> Seq.toArray

        //create structured strings for edit distance alignment; replace pipes introduced by OCR
        let tString = 
            episodeTranscriptLines
            |> Array.map( fun tl -> tl.Line.Replace(".","").Replace(",","").Replace("!","").ToUpper().Replace("|","I" ) )
            |> String.concat "|"

        let sString =
            episodeSubtitles
            |> Seq.map( fun s -> s.edited_text.ToUpper().Replace("|","I" ) )
            |> Seq.toArray
            |> String.concat "|"

        //perform edit distance alignment, save results
        // !!!!!!!!!!!!!!!!!!!
        // FOR DEBUG ONLY
        // !!!!!!!!!!!!!!!!!!!
        //fake edit distance
        //let distanceM = 0
        //let alignmentM = Array.zip (tString.ToCharArray().[0..500]) (sString.ToCharArray().[0..500])
        //commenting out for loading files to avoid edit distance recalculation
        //let alignmentSplit = System.IO.File.ReadAllText( dataDirectory + season.ToString() + "-" + (i+1+episodeOffset).ToString() + ".editalignment" ).Split('\n')
        //let alignmentM = Array.zip (alignmentSplit.[0].ToCharArray()) (alignmentSplit.[1].ToCharArray())
        let distanceM, alignmentM = getEditDistance MinimumEditDistance tString sString 
        printAlignment alignmentM season (i + 1 + episodeOffset) dataDirectory
        editDistances.Add( season.ToString() + "-" + (i + 1 + episodeOffset).ToString() + ": " + distanceM.ToString() )

        //walk the aligned strings to create an aligned object
        let mutable mi = 0
        let mutable matches = 0
        let mutable si = 0
        let mutable ti = 0
        let tempSubtitles = ResizeArray<Subtitle>()
        for tchar,uchar in alignmentM do
            //matches means non insertions now
            if uchar <> '*' && tchar <> '*' then
                matches <- matches + 1
            mi <- mi + 1
            if uchar = '|' && si < episodeSubtitles.Length then
                //avoid bad matches at beginning and end (e.g. song we couldn't clear); require some proportion are matched
                if matches > mi / 3 then
                    tempSubtitles.Add( episodeSubtitles.[si] )
                matches <- 0
                mi <- 0
                si <- si + 1
            if tchar = '|' && ti < episodeTranscriptLines.Length then 
                alignments.Add( 
                    {
                    Season=episodeTranscriptLines.[ti].Season; 
                    Episode=i+1+episodeOffset; 
		    Disc=disc
                    Character=episodeTranscriptLines.[ti].Character;
                    Turn= episodeTranscriptLines.[ti].Line ;
                    TurnIndex = ti;
                    Subtitles = tempSubtitles.ToArray()
                    } )
                tempSubtitles.Clear()
                ti <- ti + 1
        )

    let alignedJson = Newtonsoft.Json.JsonConvert.SerializeObject(alignments,Newtonsoft.Json.Formatting.Indented)
    System.IO.File.WriteAllText(filePath + ".aligned",alignedJson )
    episodeOffset <- episodeOffset + episodeSubtitles.Length
    //) |> ignore //parallel foreach
//editDistances //for Jupyter output
