

# VICE - Video, audio and Image Converter for Erlang/Elixir #

Copyright (c) 2014-2016 Grégoire Lejeune, 2016 Botsunit, 2017-2018 G-Corp

__Version:__ 0.1.0

__Authors:__ Gregoire Lejeune ([`gregoire.lejeune@gmail.com`](mailto:gregoire.lejeune@gmail.com)).

[![All Contributors](https://img.shields.io/badge/all_contributors-3-orange.svg)](#contributors)
[![Build Status](https://travis-ci.org/G-Corp/vice.svg?branch=master)](https://travis-ci.org/G-Corp/vice)
[![Hex.pm](https://img.shields.io/hexpm/v/vice.svg)](https://hex.pm/packages/vice)


### VICE ###

VICE is a fork and rewrite of [erlffmpeg](https://github.com/emedia-project/erlffmpeg).


#### Install ####

[cmake](https://cmake.org/), [ffmpeg](https://www.ffmpeg.org/)|[libav](https://www.libav.org/), [libopencv-dev](http://opencv.org/), [imagemagick](https://www.imagemagick.org/script/index.php), [SoX](http://sox.sourceforge.net/)


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


### Common options ###


<table width="100%" border="0" summary="video conversion options">
<tr><th>Type</th><th>Name</th><th>Description</th><th>Value</th><th>Example</th></tr>
<tr>
<td>global</td>
<td><tt>cgroup</tt></td>
<td>Execute encoder in the given cgroup.</td>
<td><tt>string() | binary()</tt></td>
<td><tt>{cgroup, "cpu:/erlang.vice"}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>cgexec</tt></td>
<td>Path to the <tt>cgexec</tt> executable.</td>
<td><tt>string() | binary()</tt></td>
<td><tt>{cgexec, "/usr/bin/cgexec"}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>type</tt></td>
<td>Allow you to force the input type.</td>
<td><tt>video</tt> | <tt>audio</tt> | <tt>image</tt></td>
<td><tt>{cgexec, "/usr/bin/cgexec"}</tt></td>
</tr>
</table>



### Video ###

For video conversion, VICE use [FFmpeg](https://www.ffmpeg.org).


#### Options ####


<table width="100%" border="0" summary="video conversion options">
<tr><th>Type</th><th>Name</th><th>Description</th><th>Value</th><th>Example</th></tr>
<tr>
<td>global</td>
<td><tt>preset</tt></td>
<td>Use a given preset file.</td>
<td><tt>string() | atom()</tt></td>
<td><tt>{preset, hls1080p}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>global_params</tt></td>
<td>Global parameters.</td>
<td><tt>[{atom(), term()}]</tt></td>
<td><tt>[{framerate, 25}, {bitrate, 3950}]</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>yes</tt></td>
<td>Overwrite output files without asking.</td>
<td><tt>true</tt> | <tt>false</tt></td>
<td><tt>{yes, true}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>fix_sub_duration</tt></td>
<td>Fix subtitles durations. For each subtitle, wait for the next packet in the same stream and adjust the duration of the first to avoid overlap.</td>
<td><tt>true</tt> | <tt>false</tt></td>
<td><tt>{fix_sub_duration, true}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>canvas_size</tt></td>
<td>Set the size of the canvas used to render subtitles.</td>
<td><tt>integer()</tt></td>
<td><tt>{canvas_size, 10}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>filter_complex</tt></td>
<td>Define a complex filtergraph, i.e. one with arbitrary number of inputs and/or outputs.</td>
<td><tt><a href="https://www.ffmpeg.org/ffmpeg-all.md#Filtergraph-syntax-1">Filtergraph</a> :: string() | binary() | proplist()</tt></td>
<td><tt>{filter_complex, [{"acrossfade", "d=10"}, {"c1", "exp"}, {"c2", "exp"}]}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>filter_complex_script</tt></td>
<td>This option is similar to <tt>filter_complex</tt>, the only difference is that its argument is the name of the file from which a complex filtergraph description is to be read. </td>
<td><tt>Script :: file:filename_all()</tt></td>
<td><tt>{filter_complex_script, "filter.script"}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>allowed_extensions</tt></td>
<td>List of file extensions that dash or hls is allowed to access.</td>
<td><tt>Extensions :: string() | binary()</tt></td>
<td><tt>{allowed_extensions, "vtt,aac,ts,key"}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>input_position</tt></td>
<td>Seeks in this input file to <i>position</i>.</td>
<td><tt><a href="https://www.ffmpeg.org/ffmpeg-utils.md#time-duration-syntax">position</a> :: string() | binary()</tt></td>
<td><tt>{input_position, "00:01:02.123"}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>input_eof_position</tt></td>
<td>Like the <tt>input_position</tt> option but relative to the "end of file". That is negative values are earlier in the file, 0 is at EOF. .</td>
<td><tt><a href="https://www.ffmpeg.org/ffmpeg-utils.md#time-duration-syntax">position</a> :: string() | binary()</tt></td>
<td><tt>{input_eof_position, "-0:32:01.123"}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>input_format</tt></td>
<td>Force input format.</td>
<td><tt>Format :: string()</tt></td>
<td><tt>{input_format, "mp4"}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>input_duration</tt></td>
<td>Limit the duration of data read from the input file.</td>
<td><tt><a href="https://www.ffmpeg.org/ffmpeg-utils.md#time-duration-syntax">position</a> :: string() | binary()</tt></td>
<td><tt>{input_duration, "0:32:01.123"}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>decoder</tt></td>
<td>Select a decoder for one or more streams.</td>
<td><tt>[Stream :: string(), Codec :: string()]</tt></td>
<td><tt>{decoder, ["a", "pcm_s161e"]}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>itoffset</tt></td>
<td>Set the input time offset.</td>
<td><tt><a href="https://www.ffmpeg.org/ffmpeg-utils.md#time-duration-syntax">position</a> :: string() | binary()</tt></td>
<td><tt>{itoffset, "+12.345"}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>input_frame_rate</tt></td>
<td>Set frame rate (Hz value, fraction or abbreviation).</td>
<td><tt>[Stream :: string(), FPS :: integer()] | FPS :: integer()</tt></td>
<td><tt>{input_frame_rate, 25}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>input_frame_size</tt></td>
<td>Set frame size.</td>
<td><tt>[Stream :: string(), Size :: integer()] | Size :: integer()</tt></td>
<td><tt>{input_frame_size, ["v", 1]}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>input_pixel_format</tt></td>
<td>Set pixel format.</td>
<td><tt>[Stream :: string(), Format :: string()] | Format :: string() | binary()</tt></td>
<td><tt>{input_pixel_format, "rgb24"}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>input_sws_flags</tt></td>
<td>Set SwScaler flags.</td>
<td><tt><a href="https://www.ffmpeg.org/ffmpeg-all.md#Scaler-Options">Flags</a> :: string() | binary()</tt></td>
<td><tt>{input_sws_flags, "gauss"}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>input_audio_frequency</tt></td>
<td>Set the audio sampling frequency.</td>
<td><tt>[Stream :: string(), Freq :: integer()] | Freq :: integer()</tt></td>
<td><tt>{input_audio_frequency, ["a:1", 22050]}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>input_audio_channels</tt></td>
<td>Set the number of audio channels.</td>
<td><tt>[Stream :: string(), Channels :: integer()] | Channels :: integer()</tt></td>
<td><tt>{input_audio_channels, 6}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>input_acodec</tt></td>
<td>Set the audio codec.</td>
<td><tt>Codec :: string() | binary()</tt></td>
<td><tt>{input_acodec, "pcm_s24le"}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>guess_layout_max</tt></td>
<td>If some input channel layout is not known, try to guess only if it corresponds to at most the specified number of channels.</td>
<td><tt>Channels :: integer()</tt></td>
<td><tt>{guess_layout_max, 2}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>input_vcodec</tt></td>
<td>Set the video codec.</td>
<td><tt>Codec :: string() | binary()</tt></td>
<td><tt>{input_vcodec, "libxvid"}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>input_scodec</tt></td>
<td>Set the subtitle codec.</td>
<td><tt>Codec :: string() | binary()</tt></td>
<td><tt>{input_scodec, "srt"}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>muxdelay</tt></td>
<td>Set the maximum demux-decode delay. </td>
<td><tt>Delay :: float() | integer()</tt></td>
<td><tt>{muxdelay, 0.1}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>muxpreload</tt></td>
<td>Set the initial demux-decode delay.</td>
<td><tt>Delay :: float() | integer()</tt></td>
<td><tt>{muxpreload, 0.1}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>accurate_seek</tt></td>
<td>This option enables or disables accurate seeking in input files with the <tt>output_position</tt> option.</td>
<td><tt>true | false</tt></td>
<td><tt>{accurate_seek, false}</tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>output_format</tt></td>
<td>Force output format.</td>
<td><tt>Format :: string()</tt></td>
<td><tt>{output_format, "mp4"}</tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>output_duration</tt></td>
<td>Limit the duration of data read from the output file.</td>
<td><tt><a href="https://www.ffmpeg.org/ffmpeg-utils.md#time-duration-syntax">position</a> :: string() | binary()</tt></td>
<td><tt>{output_duration, "0:32:01.123}</tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>output_position</tt></td>
<td>Seeks in this output file to <i>position</i>.</td>
<td><tt><a href="https://www.ffmpeg.org/ffmpeg-utils.md#time-duration-syntax">position</a> :: string() | binary()</tt></td>
<td><tt>{output_position, "00:01:02.123}</tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>output_eof_position</tt></td>
<td>Like the <tt>output_position</tt> option but relative to the "end of file". That is negative values are earlier in the file, 0 is at EOF. .</td>
<td><tt><a href="https://www.ffmpeg.org/ffmpeg-utils.md#time-duration-syntax">position</a> :: string() | binary()</tt></td>
<td><tt>{output_eof_position, "-0:32:01.123}</tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>encoder</tt></td>
<td>Select a encoder for one or more streams.</td>
<td><tt>[Stream :: string(), Codec :: string()] | Codec :: string()</tt></td>
<td><tt>{encoder, ["a", "pcm_s161e"]}</tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>bitrate</tt></td>
<td>Set bitrate.</td>
<td><tt>[Stream :: string(), Bitrate :: string()] | [Stream :: string(), Number :: integer(), Bitrate :: string()] | Bitrate :: string() | integer()</tt></td>
<td><tt>{bitrate, ["v", 0, "800k"]}</tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>timestamp</tt></td>
<td>Set the recording timestamp in the container.</td>
<td><tt>Date :: date()</tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>target</tt></td>
<td>Specify target file type (<tt>vcd</tt>, <tt>svcd</tt>, <tt>dvd</tt>, <tt>dv</tt>, <tt>dv50</tt>). type may be prefixed with <tt>pal-</tt>, <tt>ntsc-</tt> or <tt>film-</tt> to use the corresponding standard.</td>
<td><tt>Target :: string()</tt></td>
<td><tt>{target, "vcd"}</tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>frames</tt></td>
<td>Stop writing to the stream after framecount frames.</td>
<td><tt>[Stream :: string(), Framecount :: integer()] | Framecount :: integer()</tt></td>
<td><tt>{frames, ["v", 1]}</tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>qscale</tt></td>
<td>Use fixed quality scale (VBR).</td>
<td><tt>[Stream :: string(), Quality :: integer()] | [Stream :: string(), Number :: integer(), Quality :: integer()] | Quality :: integer()</tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>filter</tt></td>
<td>Create the filtergraph specified by filtergraph and use it to filter the stream.</td>
<td><tt>[Stream :: string(), <a href="https://www.ffmpeg.org/ffmpeg-all.md#Filtergraph-syntax-1">Filtergraph</a> :: string()]</tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>filter_script</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>pre</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>vframes</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>output_frame_rate</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>output_frame_size</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>aspect</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>no_video_recording</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>vcodec</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>pass</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>vlang</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>video_filtergraph</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>output_pixel_format</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>output_sws_flags</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>rc_override</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>top</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>force_key_frames</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>copyinkf</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>aframes</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>output_audio_frequency</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>audio_quality</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>output_audio_channels</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>no_audio_recording</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>output_acodec</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>sample_fmt</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>audio_filtergraph</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>output_scodec</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>no_subtitle_recording</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>map</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>map_channel</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>map_chapters</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>vsync</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>async</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>copytb</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>shortest</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>dts_delta_threshold</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>streamid</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>bitstream_filters</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>timecode</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>strict</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>metadata</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>disable_video</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>disable_audio</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>disable_subtitle</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>x264_profile</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>x264_level</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>x264_refs</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>start_number</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>hls_list_size</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>hls_key_info_file</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>hls_key_info</tt></td>
<td></td>
<td><tt>[{key_uri, string()}, {enc_key, string() | file:filename_all()}, {iv, string()}]</tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>hls_playlist_type</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>hls_segment_filename</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>hls_time</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
</table>



### Video Thumbnails ###

To generate video thumbnails, VICE use [ImageMagick](https://www.imagemagick.org)
<sup>[®](http://tarr.uspto.gov/servlet/tarr?regser=serial&entry=78333969)</sup>
 and [FFmpeg](https://www.ffmpeg.org). If you want to _optimize_ output images, you must add [image_optimizer](https://github.com/G-Corp/image_optimizer) in your project dependencies and install [pngquand](https://pngquant.org/) or [OptiPNG](http://optipng.sourceforge.net/).


#### Options ####


<table width="100%" border="0" summary="video conversion options">
<tr><th>Type</th><th>Name</th><th>Description</th><th>Value</th><th>Example</th></tr>
<tr>
<td>global</td>
<td><tt>every</tt></td>
<td>Generate a thumbnail _every_ seconds.</td>
<td><tt>integer()</tt></td>
<td><tt>{every, 6}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>width</tt></td>
<td>Thumbnail image width.</td>
<td><tt>integer()</tt></td>
<td><tt>{width, 100}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>out_path</tt></td>
<td>Output path.</td>
<td><tt>string() | binary()</tt></td>
<td><tt>{out_path, "my_folder"}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>assets_path</tt></td>
<td>Assets path.</td>
<td><tt>string() | binary()</tt></td>
<td><tt>{assets_path, "http://mysite.com/video/1/thumbnails"}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>sprite</tt></td>
<td>Generate a single image.</td>
<td><tt>true | false</tt></td>
<td><tt>{sprite, true}</tt></td>
</tr>
</table>



### Audio ###

For audio conversion, VICE use [SoX](http://sox.sourceforge.net/).


#### Options ####


<table width="100%" border="0" summary="video conversion options">
<tr><th>Type</th><th>Name</th><th>Description</th><th>Value</th><th>Example</th></tr>
<tr>
<td>global</td>
<td><tt>buffer</tt> or <tt>input_buffer</tt></td>
<td>Set the size in bytes of the buffers used for processing audio (default 8192).</td>
<td><tt>Bytes :: integer()</tt></td>
<td><tt>{buffer, 20000}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>clobber</tt></td>
<td>Don’t prompt before overwriting an existing file with the same name as that given for the output file.</td>
<td><tt>true | false</tt></td>
<td><tt>{clobber, false}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>combine</tt></td>
<td>Select the input file combining method.</td>
<td><tt>concatenate | merge | mix | 'mix−power' | multiply | sequence</tt></td>
<td><tt>{combine, concatenate}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>no_dither</tt></td>
<td>Disable automatic dither.</td>
<td><tt>true | false</tt></td>
<td><tt>{no_dither, true}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>effects_file</tt></td>
<td>Use <tt>File</tt> to obtain all effects and their arguments.</td>
<td><tt>File :: file:filename_all()</tt></td>
<td><tt>{effects_file, "effects.data"}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>guard</tt></td>
<td>Automatically invoke the gain effect to guard against clipping.</td>
<td><tt>true | false</tt></td>
<td><tt>{guard, true}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>magic</tt></td>
<td>If SoX has been built with the optional <tt>libmagic</tt> library then this option can be given to enable its use in helping to detect audio file types.</td>
<td><tt>true | false</tt></td>
<td><tt>{magic, true}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>multi_threaded</tt></td>
<td>By default, SoX is <i>single threaded</i>. If the <tt>multi_threaded</tt> option is given however then SoX will process audio channels for most multi-channel effects in parallel on hyper-threading/multi-core architectures.</td>
<td><tt>true | false</tt></td>
<td><tt>{multi_threaded, true}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>single_threaded</tt></td>
<td>Opposite of the <tt>multi_threaded</tt> option.</td>
<td><tt>true | false</tt></td>
<td><tt>{single_threaded, true}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>norm</tt></td>
<td>Automatically invoke the gain effect to guard against clipping and to normalise the audio.</td>
<td><tt>DBLevel :: integer() | true</tt></td>
<td><tt>{norm, true}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>play_rate_arg</tt></td>
<td>Selects a quality option to be used when the <i>rate</i> effect is automatically invoked whilst playing audio.</td>
<td><tt>Arg :: string()</tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>plot</tt></td>
<td>If not set to off (the default if <tt>plot</tt> is not given), run in a mode that can be used, in conjunction with the gnuplot program or the GNU Octave program, to assist with the selection and configuration of many of the transfer-function based effects.</td>
<td><tt>gnuplot | octave | off</tt></td>
<td><tt>{plot, gnuplot}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>repeatable</tt></td>
<td>VICE will embed a fixed time-stamp in the output file (e.g. AIFF) and will <i>seed</i> pseudo random number generators (e.g. dither) with a fixed number, thus ensuring that successive SoX invocations with the same inputs and the same parameters yield the same output.</td>
<td><tt>true | false</tt></td>
<td><tt>{repeatable, true}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>replay_gain</tt></td>
<td>Select whether or not to apply replay-gain adjustment to input files.</td>
<td><tt>track | album | off</tt></td>
<td><tt>{replay_gain, track}</tt></td>
</tr>
<tr>
<td>global</td>
<td><tt>temp</tt></td>
<td>Specify that any temporary files should be created in the given <tt>Directory</tt>.</td>
<td><tt>Directory :: string()</tt></td>
<td><tt>{temp, "/home/vice/tmp"}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>ignore_length</tt></td>
<td>Override an (incorrect) audio length given in an audio file’s header.</td>
<td><tt>true | false</tt></td>
<td><tt>{ignore_length, true}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>volume</tt></td>
<td>Intended for use when combining multiple input files, this option adjusts the volume of the file that follows it on the command line by a factor of <tt>Factor</tt>.</td>
<td><tt>{volume, Factor :: float()}</tt></td>
<td><tt>{volume, 0.8}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>input_bits</tt></td>
<td>The number of bits (a.k.a. bit-depth or sometimes word-length) in each encoded sample.</td>
<td><tt>Bytes :: integer()</tt></td>
<td><tt>{input_bits, 8}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>input_channels</tt></td>
<td>The number of audio channels in the audio file.</td>
<td><tt>Channels :: integer()</tt></td>
<td><tt>{input_channels, 2}}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>input_encoding</tt></td>
<td>The audio encoding type.</td>
<td><tt>Encoding :: string()</tt></td>
<td><tt>{input_encoding, "float"}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>input_rate</tt></td>
<td>Gives the sample rate in Hz of the file.</td>
<td><tt>Rate :: integer()</tt></td>
<td><tt>{input_rate, 48720}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>input_rate_k</tt></td>
<td>Gives the sample rate in kHz of the file.</td>
<td><tt>KRate :: integer()</tt></td>
<td><tt>{input_rate_k, 48}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>input_type</tt></td>
<td>Gives the type of the audio file.</td>
<td><tt>Filetype :: string()</tt></td>
<td><tt>{input_type, "mp3"}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>input_endian</tt></td>
<td>Specify the byte-order of the audio data.</td>
<td><tt>little | bug | swap</tt></td>
<td><tt>{input_endian, little}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>input_reverse_nibbles</tt></td>
<td>Specifies that the nibble ordering (i.e. the 2 halves of a byte) of the samples should be reversed.</td>
<td><tt>true | false</tt></td>
<td><tt>{input_reverse_nibbles, true}</tt></td>
</tr>
<tr>
<td>input</td>
<td><tt>input_reverse_bits</tt></td>
<td>Specifies that the bit ordering of the samples should be reversed.</td>
<td><tt>true | false</tt></td>
<td><tt>{input_reverse_bits, true}</tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>output_encoding</tt></td>
<td>The audio encoding type.</td>
<td><tt>Encoding :: string()</tt></td>
<td><tt>{output_encoding, "float"}</tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>output_bits</tt></td>
<td>The number of bits (a.k.a. bit-depth or sometimes word-length) in each encoded sample.</td>
<td><tt>Bytes :: integer()</tt></td>
<td><tt>{output_bits, 8}</tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>output_channels</tt></td>
<td>The number of audio channels in the audio file.</td>
<td><tt>Channels :: integer()</tt></td>
<td><tt>{output_channels, 2}}</tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>output_rate</tt></td>
<td>Gives the sample rate in Hz of the file.</td>
<td><tt>Rate :: integer()</tt></td>
<td><tt>{output_rate, 48720}</tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>output_rate_k</tt></td>
<td>Gives the sample rate in kHz of the file.</td>
<td><tt>KRate :: integer()</tt></td>
<td><tt>{output_rate_k, 48}</tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>output_type</tt></td>
<td>Gives the type of the audio file.</td>
<td><tt>Filetype :: string()</tt></td>
<td><tt>{output_type, "mp3"}</tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>output_endian</tt></td>
<td>Specify the byte-order of the audio data.</td>
<td><tt>little | bug | swap</tt></td>
<td><tt>{output_endian, little}</tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>output_reverse_nibbles</tt></td>
<td>Specifies that the nibble ordering (i.e. the 2 halves of a byte) of the samples should be reversed.</td>
<td><tt>true | false</tt></td>
<td><tt>{output_reverse_nibbles, true}</tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>output_reverse_bits</tt></td>
<td>Specifies that the bit ordering of the samples should be reversed.</td>
<td><tt>true | false</tt></td>
<td><tt>{output_reverse_bits, true}</tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>add_comment</tt></td>
<td>Append a comment in the output file header (where applicable).</td>
<td><tt>Text :: string()</tt></td>
<td><tt>{add_comment, "Encoded by VICE"}</tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>comment</tt></td>
<td>Specify the comment text to store in the output file header (where applicable).</td>
<td><tt>Text :: string()</tt></td>
<td><tt>{comment, "Encoded by VICE"}</tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>comment_file</tt></td>
<td>Specify a file containing the comment text to store in the output file header (where applicable).</td>
<td><tt>File :: file:filename_all()</tt></td>
<td><tt>{comment_file, "/home/vice/comment.txt"}</tt></td>
</tr>
<tr>
<td>output</td>
<td><tt>compression</tt></td>
<td>The compression factor for variably compressing output file formats.</td>
<td><tt>Factor :: integer()</tt></td>
<td><tt>{compression, 2}</tt></td>
</tr>
<tr>
<td>effects</td>
<td><tt>effects</tt></td>
<td>Audio effects to invoke (see <a href="http://sox.sourceforge.net/sox.md#EFFECTS">Effects</a>).</td>
<td><tt>Effects :: string()</tt></td>
<td><tt>{effects, "chorus 0.7 0.9 55 0.4 0.25 2"}</tt></td>
</tr>
</table>



### Image ###

For image conversion, VICE use [ImageMagick](https://www.imagemagick.org)
<sup>[®](http://tarr.uspto.gov/servlet/tarr?regser=serial&entry=78333969)</sup>
.


#### Options ####


<table width="100%" border="0" summary="video conversion options">
<tr><th>Type</th><th>Name</th><th>Description</th><th>Value</th><th>Example</th></tr>
<tr>
<td>convert</td>
<td><tt>resize</tt></td>
<td>Resize image</td>
<td><tt>{resize, P :: integer(), percent | pixel} | {resize, W :: integer(), H :: integer()} | {resize, W :: integer(), H :: integer(), percent | ignore_ration | no_enlarge | no_shrink | fill}</tt></td>
<td><tt>{resize, 50, percent}</tt></td>
</tr>
<tr>
<td>convert</td>
<td><tt>thumbnail</tt></td>
<td>Create a thumbnail of the image. This is similar to <tt>resize</tt>, except it is optimized for speed and any image profile, other than a color profile, is removed to reduce the thumbnail size.</td>
<td><tt>{thumbnail, P :: integer(), percent | pixel} | {thumbnail, W :: integer(), H :: integer()} | {thumbnail, W :: integer(), H :: integer(), percent | ignore_ration | no_enlarge | no_shrink | fill}</tt></td>
<td><tt>{thumbnail, 100, 100, no_enlarge}</tt></td>
</tr>
<tr>
<td>convert</td>
<td><tt>quality</tt></td>
<td></td>
<td><tt></tt></td>
<td><tt></tt></td>
</tr>
<tr>
<td>convert</td>
<td><tt>crop</tt></td>
<td>Cut out a rectangular region of the image.</td>
<td><tt>{crop, W :: integer(), H :: integer(), X :: integer(), Y :: integer()} | {crop, W :: integer(), H :: integer()}</tt></td>
<td><tt>{crop, 100, 80, 20, 30}</tt></td>
</tr>
<tr>
<td>convert</td>
<td><tt>gravity</tt></td>
<td>Sets the current gravity suggestion for various other settings and options. Choices include: <tt>NorthWest</tt>, <tt>North</tt>, <tt>NorthEast</tt>, <tt>West</tt>, <tt>Center</tt>, <tt>East</tt>, <tt>SouthWest</tt>, <tt>South</tt>, <tt>SouthEast</tt></td>
<td><tt>{gravity, Gravity :: string()}</tt></td>
<td><tt>{gravity, "SouthEast"}</tt></td>
</tr>
<tr>
<td>convert</td>
<td><tt>repage</tt> or <tt>'+repage'</tt></td>
<td>Adjust the canvas and offset information of the image</td>
<td>-</td>
<td><tt>repage</tt></td>
</tr>
<tr>
<td>convert</td>
<td><tt>flip</tt></td>
<td>Create a mirror image</td>
<td>-</td>
<td><tt>flip</tt></td>
</tr>
<tr>
<td>convert</td>
<td><tt>trim</tt></td>
<td>Trim an image</td>
<td>-</td>
<td><tt>trim</tt></td>
</tr>
<tr>
<td>convert</td>
<td><tt>magnify</tt></td>
<td>Double the size of the image with pixel art scaling</td>
<td>-</td>
<td><tt>magnify</tt></td>
</tr>
<tr>
<td>convert</td>
<td><tt>rotation</tt></td>
<td>Apply Paeth image rotation (using shear operations) to the image</td>
<td><tt>{rotation, Degrees :: integer()}</tt></td>
<td><tt>{rotation, 90}</tt></td>
</tr>
<tr>
<td>convert</td>
<td><tt>'auto-orient'</tt></td>
<td>Adjusts an image so that its orientation is suitable for viewing</td>
<td>-</td>
<td><tt>'auto-orient'</tt></td>
</tr>
<tr>
<td>convert</td>
<td><tt>strip</tt></td>
<td>Strip the image of any profiles, comments or these PNG chunks: bKGD,cHRM,EXIF,gAMA,iCCP,iTXt,sRGB,tEXt,zCCP,zTXt,date.</td>
<td>-</td>
<td><tt>strip</tt></td>
</tr>
<tr>
<td>convert</td>
<td><tt>blur</tt></td>
<td>Reduce image noise and reduce detail levels</td>
<td><tt>{blur, Radius :: integer()} | {blur, Radius :: integer(), Sigma :: integer()}</tt></td>
<td><tt>{blur, 0, 8}</tt></td>
</tr>
<tr>
<td>convert</td>
<td><tt>edge</tt></td>
<td>Detect edges within an image</td>
<td><tt>{edge, Radius :: integer()}</tt></td>
<td><tt>{edge 3}</tt></td>
</tr>
<tr>
<td>convert</td>
<td><tt>size</tt></td>
<td>Set the width and height of the image</td>
<td><tt>{size, Width} | {size, Width :: integer(), Height :: integer()} | {size, Width :: integer(), Height :: integer(), Offset :: integer()}</tt></td>
<td><tt>{size, 192, 128}</tt></td>
</tr>
<tr>
<td>convert</td>
<td><tt>extent</tt></td>
<td>Set the image size and offset</td>
<td><tt>{extent, Width :: integer(), Height :: integer()}</tt></td>
<td><tt>{extent, 384, 256}</tt></td>
</tr>
<tr>
<td>mogrify</td>
<td><tt>geometry</tt></td>
<td>Set the preferred size and location of the image.</td>
<td><tt>{geometry, Scale :: integer(), percent} | {geometry, Area :: integer(), pixels} | {geometry, ScaleX :: integer(), ScaleY :: integer(), percent} | {geometry, Width :: integer()} | {geometry, Width :: integer(), undefined} | {geometry, undefined, Height :: integer()} | {geometry, Width :: integer(), Height :: integer()} | {geometry, Width :: integer(), Height :: integer(), ignore_ration | no_enlarge | no_shrink | fill} | {geometry, Width :: integer(), Height :: integer(), X :: integer(), Y :: integer()}</tt></td>
<td><tt>{geometry, 325, 192, 10, 10}</tt></td>
</tr>
<tr>
<td>montage</td>
<td><tt>geometry</tt></td>
<td>Set the preferred size and location of the image.</td>
<td><tt>{geometry, Scale :: integer(), percent} | {geometry, Area :: integer(), pixels} | {geometry, ScaleX :: integer(), ScaleY :: integer(), percent} | {geometry, Width :: integer()} | {geometry, Width :: integer(), undefined} | {geometry, undefined, Height :: integer()} | {geometry, Width :: integer(), Height :: integer()} | {geometry, Width :: integer(), Height :: integer(), ignore_ration | no_enlarge | no_shrink | fill} | {geometry, Width :: integer(), Height :: integer(), X :: integer(), Y :: integer()}</tt></td>
<td><tt>{geometry, 10, percent}</tt></td>
</tr>
<tr>
<td>montage</td>
<td><tt>tile</tt></td>
<td>Specify the layout of images.</td>
<td><tt>{tile, Scale :: integer(), percent} | {tile, Area :: integer(), pixels} | {tile, ScaleX :: integer(), ScaleY :: integer(), percent} | {tile, Width :: integer()} | {tile, Width :: integer(), undefined} | {tile, undefined, Height :: integer()} | {tile, Width :: integer(), Height :: integer()} | {tile, Width :: integer(), Height :: integer(), ignore_ration | no_enlarge | no_shrink | fill} | {tile, Width :: integer(), Height :: integer(), X :: integer(), Y :: integer()}</tt></td>
<td><tt>{tile, 234, 186, ignore_ration}</tt></td>
</tr>
</table>




### Licence ###

VICE is available for use under the following license, commonly known as the 3-clause (or "modified") BSD license:

Copyright (c) 2014-2015 Grégoire Lejeune<br />
Copyright (c) 2016 BotsUnit<br />
Copyright (c) 2017-2018 G-Corp<br />

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
* Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
* The name of the author may not be used to endorse or promote products derived from this software without specific prior written permission.



THIS SOFTWARE IS PROVIDED BY THE AUTHOR ''AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.



## Modules ##


<table width="100%" border="0" summary="list of modules">
<tr><td><a href="https://github.com/G-Corp/vice/blob/master/doc/vice.md" class="module">vice</a></td></tr>
<tr><td><a href="https://github.com/G-Corp/vice/blob/master/doc/vice_encoder.md" class="module">vice_encoder</a></td></tr>
<tr><td><a href="https://github.com/G-Corp/vice/blob/master/doc/vice_subtitles.md" class="module">vice_subtitles</a></td></tr>
<tr><td><a href="https://github.com/G-Corp/vice/blob/master/doc/vice_thumbnails.md" class="module">vice_thumbnails</a></td></tr></table>

## Contributors ##

Thanks goes to these wonderful people ([emoji key](https://github.com/kentcdodds/all-contributors#emoji-key)):

<!-- ALL-CONTRIBUTORS-LIST:START - Do not remove or modify this section -->
<!-- prettier-ignore -->
<table><tr><td align="center"><a href="http://lejeun.es"><img src="https://avatars1.githubusercontent.com/u/15168?v=4" width="100px;" alt="Gregoire Lejeune"/><br /><sub><b>Gregoire Lejeune</b></sub></a><br /><a href="https://github.com/G-Corp/vice/commits?author=glejeune" title="Code">💻</a> <a href="#design-glejeune" title="Design">🎨</a> <a href="https://github.com/G-Corp/vice/commits?author=glejeune" title="Documentation">📖</a> <a href="https://github.com/G-Corp/vice/commits?author=glejeune" title="Tests">⚠️</a></td><td align="center"><a href="https://github.com/agombault"><img src="https://avatars2.githubusercontent.com/u/7051246?v=4" width="100px;" alt="Augustin G"/><br /><sub><b>Augustin G</b></sub></a><br /><a href="https://github.com/G-Corp/vice/commits?author=agombault" title="Code">💻</a> <a href="https://github.com/G-Corp/vice/commits?author=agombault" title="Tests">⚠️</a></td><td align="center"><a href="http://twitter.com/sulphur27"><img src="https://avatars2.githubusercontent.com/u/186265?v=4" width="100px;" alt="P"/><br /><sub><b>P</b></sub></a><br /><a href="https://github.com/G-Corp/vice/commits?author=sulphur" title="Code">💻</a> <a href="https://github.com/G-Corp/vice/commits?author=sulphur" title="Documentation">📖</a></td></tr></table>

<!-- ALL-CONTRIBUTORS-LIST:END -->

This project follows the [all-contributors](https://github.com/kentcdodds/all-contributors) specification. Contributions of any kind welcome!
