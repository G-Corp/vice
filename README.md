

# VICE - Video, audio and Image Converter for Erlang/Elixir #

Copyright (c) 2014-2016 Grégoire Lejeune, 2016 Botsunit, 2017 G-Corp

__Version:__ 0.0.3

__Authors:__ Gregoire Lejeune ([`gregoire.lejeune@gmail.com`](mailto:gregoire.lejeune@gmail.com)).

[![Build Status](https://travis-ci.org/G-Corp/vice.svg?branch=master)](https://travis-ci.org/G-Corp/vice)
[![Hex.pm](https://img.shields.io/hexpm/v/vice.svg)](https://hex.pm/packages/vice)


### VICE ###

VICE is a fork and rewrite of [erlffmpeg](https://github.com/emedia-project/erlffmpeg).


#### Install ####

[cmake](https://cmake.org/), [ffmpeg](https://www.ffmpeg.org/)|[libav](https://www.libav.org/), [libopencv-dev](http://opencv.org/), [imagemagick](https://www.imagemagick.org/script/index.php)


#### Example ####

```

1> vice:start().
...
2> {async, Worker} = vice:convert("test/erlang.mp4", "erlang.webm").
...
3> vice:status(Worker).
{running, 9.131803868645973}
3> vice:status(Worker).
{running, 22.89698605488079}
...

```


### Video ###

For video conversion, VICE use ffmpeg.


#### Main options ####

`{output_format, Format}`<br />
`{input_format, Format}`<br />
: Force input or output file format. The format is normally auto detected for input files and guessed from the file extension for output files, so this option is not needed in most cases.

`{yes, true}`<br />
: Overwrite output files without asking.

`{encoder, [Stream, Codec]}` | `{encoder, Codec}`<br />
: Select and encoder for one or more output streams.

`{decoder, [Stream, Codec]}` | `{decoder, Codec}`<br />
: Select a decoder for one or more input streams.

`{duration, Duration}`<br />
: Stop writing the output after its duration reaches duration. Duration may be a number in seconds, or in "hh:mm:ss[.xxx]" form.

`{limit_size, Limit}`<br />
: Set the file size limit. Limit expressed in bytes.

`{input_position, Position}`<br />
: Seeks in the input file to position. Position may be either in seconds or in "hh:mm:ss[.xxx]" form.

`{output_position, Position}`<br />
: Decodes but discards input until the timestamps reach position. Position may be either in seconds or in "hh:mm:ss[.xxx]" form.

`{itoffset, Offset}`<br />
: Set the input time offset in seconds. "[-]hh:mm:ss[.xxx]" syntax is also supported. The offset is added to the timestamps of the input files.

`{timestamp, Time}`<br />
: Set the recording timestamp in the container.  The syntax for time is: `now|([(YYYY-MM-DD|YYYYMMDD)[T|t| ]]((HH:MM:SS[.m...])|(HHMMSS[.m...]))[Z|z])`

`{target, Type}`<br />
: Specify target file type ("vcd", "svcd", "dvd", "dv", "dv50"). Type may be prefixed with "pal-", "ntsc-" or "film-" to use the corresponding standard.

`{dframes, Number}`<br />
: Set the number of data frames to record.

`{frames, [Stream, Framecount]}` | `{frames, Framecount}`<br />
: Stop writing to the Stream after Framecount frames.

`{qscale, [Stream, Quality]}` | `{qscale, Quality}`<br />
: Use fixed quality scale (VBR).

`{filter, [Stream, Filtergraph]}` | `{filter, Filtergraph}`<br />
: Create the filtergraph specified by Siltergraph and use it to filter the Stream. (see ffmpeg-filters manual for more information about the filtergraph syntax.)

`{filter_script, [Stream, Filename]}` | `{filter_script, Filename}`<br />
: This option is similar to filter, the only difference is that its argument is the name of the file from which a filtergraph description is to be read.

`{pre, [Stream, PresetName]}` | `{pre, PresetName}`<br />
: Specify the preset for matching stream(s).


#### Video Options ####

`{vframes, Number}`<br />
: Set the number of video frames to record. This is an alias for "-frames:v".

`{input_frame_rate, [Stream, Fps]}` | `{input_frame_rate, Fps}`<br />
`{output_frame_rate, [Stream, Fps]}` | `{output_frame_rate, Fps}`<br />
: Set frame rate (Hz value, fraction or abbreviation).

`{input_frame_size, [Stream, Size]}` | `{input_frame_size, Size}`<br />
`{output_frame_size, [Stream, Size]}` | `{output_frame_size, Size}`<br />
: Set frame size.

`{aspect, [Stream, Aspect]}` | `{aspect, Aspect}`<br />
: Set the video display aspect ratio specified by aspect. Aspect can be a floating point number string, or a string of the form num:den, where num and den are the numerator and denominator of the aspect ratio. For example "4:3", "16:9", "1.3333", and "1.7777" are valid argument values.

`{no_video_recording, true}`<br />
: Disable video recording.

`{vcodev, Codec}`<br />
: Set the video codec.

`{pass, [Stream, N]}` | `{pass, N}`<br />
: Select the pass number (1 or 2).

`{vlang, Code}`<br />
: Set the ISO 639 language code (3 letters) of the current video stream.

`{video_filtergraph, Filter}`<br />
: Create the filtergraph specified by filtergraph and use it to filter the stream.


#### Advanced Video Options ####

`{input_pixel_format, [Stream, Format]}` | `{input_pixel_format, Format}`<br />
`{output_pixel_format, [Stream, Format]}` | `{output_pixel_format, Format}`<br />
: Set pixel format.

`{input_sws_flags, Flags}`<br />
`{output_sws_flags, Flags}`<br />
: Set SwScaler flags.

`{rc_override, [Stream, Override]}` | `{rc_override, Override}`<br />
: Rate control override for specific intervals

`{top, [Stream, N]}` | `{top, N}`<br />
: top=1/bottom=0/auto=-1 field first

`‘force_key_frames, [Stream, KeyFrame]}` | `{force_key_frames, KeyFrame}`<br />
: Force key frames at the specified timestamps, more precisely at the first frames after each specified time.

`{copyinkf, [Stream, true]}` | `{copyinkf, true}`<br />
: When doing stream copy, copy also non-key frames found at the beginning.


#### Audio Options ####

`{aframes, N}`<br />
: Set the number of audio frames to record.

`{input_audio_frequency, [Stream, Frequency]}` | `{input_audio_frequency, Frequency}`<br />
`{output_audio_frequency, [Stream, Frequency]}` | `{output_audio_frequency, Frequency}`<br />
: Set the audio sampling frequency.

`{audio_quality, Quality}`<br />
: Set the audio quality (codec-specific, VBR).

`{input_audio_channels, [Stream, N]}` | `{input_audio_channels, N}`<br />
`{output_audio_channels, [Stream, N]}` | `{output_audio_channels, N}`<br />
: Set the number of audio channels.

`{no_audio_recording, true}`<br />
: Disable audio recording.

`{input_acodec, Codec}`<br />
`{output_acodec, Codec}`<br />
Set the audio codec.

`{sample_fmt, [Stream, AudioSampleFormat]}` | `{sample_fmt, AudioSampleFormat}`<br />
Set the audio sample format.

`{audio_filtergraph, Filter}`<br />
Create the filtergraph specified by filtergraph and use it to filter the stream.


#### Advanced Audio options: ####

`{guess_layout_max, Channels}`<br />
: If some input channel layout is not known, try to guess only if it corresponds to at most the specified number of channels.


#### Subtitle options: ####

`{input_scodec, Codec}`<br />
`{output_scodec, Codec}`<br />
: Set the subtitle codec.

`{no_subtitle_recording, true}`<br />
: Disable subtitle recording.


#### Advanced Subtitle options: ####

`{fix_sub_duration, true}`<br />
: Fix subtitles durations.

`{canvas_size, Size}`<br />
: Set the size of the canvas used to render subtitles.


#### Advanced options ####

`{map, Map}`<br />
: Designate one or more input streams as a source for the output file.

`{map_channel, MapChannel}`<br />
: Map an audio channel from a given input to an output.

`{map_chapters, InputFileIndex}`<br />
: Copy chapters from input file with index input_file_index to the next output file.

`{vsync, Parameter}`<br />
: Video sync method.

`{async, SamplePerSec}`<br />
: Audio sync method.

`{copytb, Mode}`<br />
: Specify how to set the encoder timebase when stream copying.

`{shortest, true}`<br />
: Finish encoding when the shortest input stream ends.

`{dts_delta_threshold, true}`<br />
: Timestamp discontinuity delta threshold.

`{muxdelay, Second}`<br />
: Set the maximum demux-decode delay.

`{muxpreload, Second}`<br />
: Set the initial demux-decode delay.

`{streamid, OSI_NV}`<br />
: Assign a new stream-id value to an output stream.

`{bitstream_filters, [Stream, Filters]}` | `{bitstream_filters, Filters}`<br />
: Set bitstream filters for matching streams.

`{timecode, Timecode}`<br />
: Specify Timecode for writing.

`{filter_complex, Filtergraph}`<br />
: Define a complex filtergraph, i.e. one with arbitrary number of inputs and/or outputs.

`{filter_complex_script, Filename}`<br />
: This option is similar to filter_complex, the only difference is that its argument is the name of the file from which a complex filtergraph description is to be read.

`{accurate_seek, true}`<br />
: This option enables or disables accurate seeking in input files with the input_position option.


### Image ###

For image conversion, VICE use ImageMagick.

`{resize, P, percent}`
`{resize, P, pixels}` |<br />
`{resize, W, H}` |<br />
`{resize, W, H, percent}` |<br />
`{resize, W, H, ignore_ration}` |<br />
`{resize, W, H, no_enlarge}` |<br />
`{resize, W, H, no_shrink}` |<br />
`{resize, W, H, fill}`<br />
: Resize an image

`{thumbnail, P, percent}` |<br />
`{thumbnail, P, pixels}` |<br />
`{thumbnail, W, H}` |<br />
`{thumbnail, W, H, percent}` |<br />
`{thumbnail, W, H, ignore_ration}` |<br />
`{thumbnail, W, H, no_enlarge}` |<br />
`{thumbnail, W, H, no_shrink}` |<br />
`{thumbnail, W, H, fill}`<br />
: Create a thumbnail of the image. This is similar to `resize`, except it is optimized for speed and any image profile, other than a color profile, is removed to reduce the thumbnail size.

`{crop, W, H, X, Y}` |<br />
`{crop, W, H}`<br />
: Cut out a rectangular region of the image

`{gravity, Gravity}`<br />
: Sets the current gravity suggestion for various other settings and options. Choices include: `NorthWest`, `North`, `NorthEast`, `West`, `Center`, `East`, `SouthWest`, `South`, `SouthEast`.

`repage` |<br />
`'+repage'`<br />
: Adjust the canvas and offset information of the image

`flip`<br />
: Create a mirror image

`trim`<br />
: Trim an image

`magnify`<br />
: Double the size of the image with pixel art scaling

`{rotate, Degrees}`<br />
: Apply Paeth image rotation (using shear operations) to the image

`'auto-orient'`<br />
: Adjusts an image so that its orientation is suitable for viewing

`strip`<br />
: Strip the image of any profiles, comments or these PNG chunks: bKGD,cHRM,EXIF,gAMA,iCCP,iTXt,sRGB,tEXt,zCCP,zTXt,date.

`{blur, Radius}` |<br />
`{blur, Radius, Sigma}`<br />
: Reduce image noise and reduce detail levels

`{edge, Radius}`<br />
: Detect edges within an image

`{size, Weight}` |<br />
`{size, Weight, Height}` |<br />
`{size, Weight, Height, Offset}`<br />
: Set the width and height of the image

`{extent, W, H}`<br />
: Set the image size and offset


### Licence ###

VICE is available for use under the following license, commonly known as the 3-clause (or "modified") BSD license:

Copyright (c) 2014-2015 Grégoire Lejeune<br />
Copyright (c) 2016 BotsUnit<br />
Copyright (c) 2017 G-Corp<br />

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
* The name of the author may not be used to endorse or promote products derived from this software without specific prior written permission.



THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/G-Corp/vice/blob/greg/settings/doc/vice.md" class="module">vice</a></td></tr>
<tr><td><a href="https://github.com/G-Corp/vice/blob/greg/settings/doc/vice_subtitles.md" class="module">vice_subtitles</a></td></tr></table>

