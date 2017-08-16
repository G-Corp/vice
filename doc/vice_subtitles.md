

# Module vice_subtitles #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-subs">subs()</a> ###


<pre><code>
subs() = #{}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#parse-1">parse/1</a></td><td>
Parse a subtitle string.</td></tr><tr><td valign="top"><a href="#parse_file-1">parse_file/1</a></td><td>
Parse a subtitle file.</td></tr><tr><td valign="top"><a href="#to_file-2">to_file/2</a></td><td>Equivalent to <a href="#to_file-3"><tt>to_file(Subs, File, #{})</tt></a>.</td></tr><tr><td valign="top"><a href="#to_file-3">to_file/3</a></td><td> 
Generate a subtitle file.</td></tr><tr><td valign="top"><a href="#to_file-4">to_file/4</a></td><td> 
Generate a subtitle file in the given format.</td></tr><tr><td valign="top"><a href="#to_string-2">to_string/2</a></td><td>Equivalent to <a href="#to_string-3"><tt>to_string(Subs, Type, #{})</tt></a>.</td></tr><tr><td valign="top"><a href="#to_string-3">to_string/3</a></td><td> 
Generate a subtitle string.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="parse-1"></a>

### parse/1 ###

<pre><code>
parse(Data::string()) -&gt; {ok, <a href="#type-subs">subs()</a>} | {error, {integer(), integer()}}
</code></pre>
<br />

Parse a subtitle string

<a name="parse_file-1"></a>

### parse_file/1 ###

<pre><code>
parse_file(File::<a href="file.md#type-filename_all">file:filename_all()</a>) -&gt; {ok, <a href="#type-subs">subs()</a>} | {error, term()} | {error, {integer(), integer()}}
</code></pre>
<br />

Parse a subtitle file

<a name="to_file-2"></a>

### to_file/2 ###

<pre><code>
to_file(Subs::<a href="#type-subs">subs()</a>, File::<a href="file.md#type-filename_all">file:filename_all()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Equivalent to [`to_file(Subs, File, #{})`](#to_file-3).

<a name="to_file-3"></a>

### to_file/3 ###

<pre><code>
to_file(Subs::<a href="#type-subs">subs()</a>, File::<a href="file.md#type-filename_all">file:filename_all()</a>, Options::#{}) -&gt; ok | no_data | {error, term()}
</code></pre>
<br />


Generate a subtitle file.

This function will use the file extension to determine the subtitle format.

M3U8 Options:

* segment_time

* segment_filename


SRT and WEBVTT options:

* from

* to

* duration


<a name="to_file-4"></a>

### to_file/4 ###

<pre><code>
to_file(Subs::<a href="#type-subs">subs()</a>, File::<a href="file.md#type-filename_all">file:filename_all()</a>, Options::#{}, Type::webvtt | srt | m3u8) -&gt; ok | no_data | {error, term()}
</code></pre>
<br />


Generate a subtitle file in the given format.

M3U8 Options:

* segment_time

* segment_filename


SRT and WEBVTT options:

* from

* to

* duration


<a name="to_string-2"></a>

### to_string/2 ###

<pre><code>
to_string(Subs::<a href="#type-subs">subs()</a>, Type::webvtt | srt) -&gt; {ok, string(), integer(), float()} | no_data
</code></pre>
<br />

Equivalent to [`to_string(Subs, Type, #{})`](#to_string-3).

<a name="to_string-3"></a>

### to_string/3 ###

<pre><code>
to_string(Subs::<a href="#type-subs">subs()</a>, Type::srt | webvtt, Options::#{}) -&gt; {ok, string(), integer(), float()} | no_data
</code></pre>
<br />


Generate a subtitle string

Options:

* from

* to

* duration


