

# Module vice #
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#convert-2">convert/2</a></td><td>Equivalent to <a href="#convert-4"><tt>convert(In, Out, [], undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#convert-3">convert/3</a></td><td>
Convert a media.</td></tr><tr><td valign="top"><a href="#convert-4">convert/4</a></td><td>
Convert a media.</td></tr><tr><td valign="top"><a href="#info-2">info/2</a></td><td>
Return the given media informations.</td></tr><tr><td valign="top"><a href="#infos-1">infos/1</a></td><td>
Return the media informations.</td></tr><tr><td valign="top"><a href="#screenshot-2">screenshot/2</a></td><td>
Create a screenshot for a movie.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>
Start vice application.</td></tr><tr><td valign="top"><a href="#status-1">status/1</a></td><td>
Return the conversion status.</td></tr><tr><td valign="top"><a href="#to.md5_mp4-2">to_html5_mp4/2</a></td><td>Equivalent to <a href="#to.md5_mp4-3"><tt>to_html5_mp4(Input, Output, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#to.md5_mp4-3">to_html5_mp4/3</a></td><td>
Convert the given movie for mp5 html5.</td></tr><tr><td valign="top"><a href="#to.md5_ogg-2">to_html5_ogg/2</a></td><td>Equivalent to <a href="#to.md5_ogg-3"><tt>to_html5_ogg(Input, Output, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#to.md5_ogg-3">to_html5_ogg/3</a></td><td>
Convert the given movie for ogg html5.</td></tr><tr><td valign="top"><a href="#to.md5_webm-2">to_html5_webm/2</a></td><td>Equivalent to <a href="#to.md5_webm-3"><tt>to_html5_webm(Input, Output, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#to.md5_webm-3">to_html5_webm/3</a></td><td>
Convert the given movie for webm html5.</td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td>
Return the file type (image or video).</td></tr><tr><td valign="top"><a href="#webvtt-2">webvtt/2</a></td><td>Equivalent to <a href="#webvtt-3"><tt>webvtt(Movie, OutName,
[{every, 1}, {width, 100}, {out_path, "."},
{sprite, true}])</tt></a>.</td></tr><tr><td valign="top"><a href="#webvtt-3">webvtt/3</a></td><td> 
Generate a video thumbnails (.vtt + sprite).</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="convert-2"></a>

### convert/2 ###

`convert(In, Options) -> any()`

Equivalent to [`convert(In, Out, [], undefined)`](#convert-4).

<a name="convert-3"></a>

### convert/3 ###

`convert(In, Options, Fun) -> any()`

Convert a media

<a name="convert-4"></a>

### convert/4 ###

<pre><code>
convert(In::binary() | string(), Out::binary() | string() | undefined, Options::list(), Fun::{fun((term()) -&gt; term()) | fun((term(), term()) -&gt; term()), term()} | fun((term()) -&gt; term()) | fun(() -&gt; term()) | sync | undefined) -&gt; {async, term()} | {ok, In::binary() | string(), Out::binary() | string()} | {error, term()}
</code></pre>
<br />

Convert a media

<a name="info-2"></a>

### info/2 ###

`info(File, Info) -> any()`

Return the given media informations

<a name="infos-1"></a>

### infos/1 ###

`infos(File) -> any()`

Return the media informations

<a name="screenshot-2"></a>

### screenshot/2 ###

<pre><code>
screenshot(Movie::binary() | string(), Out::binary() | string()) -&gt; {ok, Movie::binary() | string(), Out::binary() | string()} | {error, term()}
</code></pre>
<br />

Create a screenshot for a movie

<a name="start-0"></a>

### start/0 ###

`start() -> any()`

Start vice application

<a name="status-1"></a>

### status/1 ###

`status(Worker) -> any()`

Return the conversion status

<a name="to_html5_mp4-2"></a>

### to_html5_mp4/2 ###

`to_html5_mp4(Input, Output) -> any()`

Equivalent to [`to_html5_mp4(Input, Output, undefined)`](#to.md5_mp4-3).

<a name="to_html5_mp4-3"></a>

### to_html5_mp4/3 ###

`to_html5_mp4(Input, Output, Fun) -> any()`

Convert the given movie for mp5 html5

<a name="to_html5_ogg-2"></a>

### to_html5_ogg/2 ###

`to_html5_ogg(Input, Output) -> any()`

Equivalent to [`to_html5_ogg(Input, Output, undefined)`](#to.md5_ogg-3).

<a name="to_html5_ogg-3"></a>

### to_html5_ogg/3 ###

`to_html5_ogg(Input, Output, Fun) -> any()`

Convert the given movie for ogg html5

<a name="to_html5_webm-2"></a>

### to_html5_webm/2 ###

`to_html5_webm(Input, Output) -> any()`

Equivalent to [`to_html5_webm(Input, Output, undefined)`](#to.md5_webm-3).

<a name="to_html5_webm-3"></a>

### to_html5_webm/3 ###

`to_html5_webm(Input, Output, Fun) -> any()`

Convert the given movie for webm html5

<a name="type-1"></a>

### type/1 ###

`type(File) -> any()`

Return the file type (image or video)

<a name="webvtt-2"></a>

### webvtt/2 ###

`webvtt(Movie, OutName) -> any()`

Equivalent to [`webvtt(Movie, OutName,[{every, 1}, {width, 100}, {out_path, "."},{sprite, true}])`](#webvtt-3).

<a name="webvtt-3"></a>

### webvtt/3 ###

<pre><code>
webvtt(Movie::binary() | string(), OutName::binary() | string(), Options::list()) -&gt; ok | {error, term()}
</code></pre>
<br />


Generate a video thumbnails (.vtt + sprite)

Options:

* `every :: inetegr()`

* `width :: integer()`

* `out_path :: string()`

* `sprite :: true | false`


