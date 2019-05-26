# SouthParkTTSData

Jupyter/SoS notebook to generate TTS data from South Park DVDs in LJSpeech format.

The purpose of this [SoS notebook](https://vatlab.github.io/sos-docs/) is to prepare a dataset for learning TTS from South Park video data.

The general pipeline is applicable to creating TTS data when subtitles are available, *except* for the speaker identification component, which uses South Park scripts.
It's possible that the script method of speaker identification used here would translate to other datasets with some nontrivial modification.

Creating the South Park TTS dataset involved the following steps:

1. Convert DVDs
2. Extract subtitles
3. Align subtitles with audio sufficiently for labeled data needs
4. Do speaker identification
5. Create WAV/transcript data in suitable format (e.g. [LJSpeech](https://keithito.com/LJ-Speech-Dataset/))

Please see the notebook for an elaboration of these steps and related dependencies (JupyterLab, SoS, and ifSharp/F# kernel are needed to get up and running). 

For obvious copyright reasons, the processed data can't be shared.