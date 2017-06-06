

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
Parse a subtitle file.</td></tr><tr><td valign="top"><a href="#to_file-2">to_file/2</a></td><td> 
Generate a subtitle file.</td></tr><tr><td valign="top"><a href="#to_file-3">to_file/3</a></td><td>
Generate a subtitle file in the given format.</td></tr><tr><td valign="top"><a href="#to_string-2">to_string/2</a></td><td>
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

`to_file(Subs, File) -> any()`


Generate a subtitle file.

This function will use the file extension to determine the subtitle format.

<a name="to_file-3"></a>

### to_file/3 ###

<pre><code>
to_file(Subs::<a href="#type-subs">subs()</a>, Type::srt | webvtt, File::<a href="file.md#type-filename_all">file:filename_all()</a>) -&gt; ok | {error, term()}
</code></pre>
<br />

Generate a subtitle file in the given format.

<a name="to_string-2"></a>

### to_string/2 ###

<pre><code>
to_string(Subs::<a href="#type-subs">subs()</a>, Type::srt | webvtt) -&gt; {ok, string()} | {error, term()}
</code></pre>
<br />

Generate a subtitle string

