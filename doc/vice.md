

# Module vice #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="types"></a>

## Data Types ##




### <a name="type-info_option">info_option()</a> ###


<pre><code>
info_option() = {labels, atom | binary} | {type, image | audio | video} | {allowed_extensions, string()}
</code></pre>




### <a name="type-info_options">info_options()</a> ###


<pre><code>
info_options() = [<a href="#type-info_option">info_option()</a>] | #{labels =&gt; atom | binary, type =&gt; image | audio | video, allowed_extensions =&gt; string()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#convert-2">convert/2</a></td><td>Equivalent to <a href="#convert-4"><tt>convert(In, Out, [], undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#convert-3">convert/3</a></td><td>
Convert a media.</td></tr><tr><td valign="top"><a href="#convert-4">convert/4</a></td><td>
Convert a media.</td></tr><tr><td valign="top"><a href="#info-2">info/2</a></td><td>Equivalent to <a href="#info-3"><tt>info(File, Info, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#info-3">info/3</a></td><td>
Return the given media information.</td></tr><tr><td valign="top"><a href="#infos-1">infos/1</a></td><td>Equivalent to <a href="#infos-2"><tt>infos(File, [])</tt></a>.</td></tr><tr><td valign="top"><a href="#infos-2">infos/2</a></td><td>
Return all media informations.</td></tr><tr><td valign="top"><a href="#screenshot-2">screenshot/2</a></td><td>
Create a screenshot for a movie.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>
Start vice application.</td></tr><tr><td valign="top"><a href="#status-1">status/1</a></td><td>
Return the conversion status.</td></tr><tr><td valign="top"><a href="#stop-0">stop/0</a></td><td>
Stop vice application.</td></tr><tr><td valign="top"><a href="#stop-1">stop/1</a></td><td>
Stop a running job.</td></tr><tr><td valign="top"><a href="#thumbnails-2">thumbnails/2</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#thumbnails-3">thumbnails/3</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#to.md5_mp4-2">to_html5_mp4/2</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#to.md5_mp4-3">to_html5_mp4/3</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#to.md5_ogg-2">to_html5_ogg/2</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#to.md5_ogg-3">to_html5_ogg/3</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#to.md5_webm-2">to_html5_webm/2</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#to.md5_webm-3">to_html5_webm/3</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#type-1">type/1</a></td><td>
Return the file type (image, audio or video).</td></tr><tr><td valign="top"><a href="#webvtt-2">webvtt/2</a></td><td>(<em>Deprecated</em>.) </td></tr><tr><td valign="top"><a href="#webvtt-3">webvtt/3</a></td><td>(<em>Deprecated</em>.) </td></tr></table>


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
convert(In::binary() | string() | [binary() | string()], Out::binary() | string() | undefined, Options::list(), Fun::{fun((Data::term()) -&gt; term()) | fun(({ok, In::string() | binary(), Out::string() | binary()} | {error, In::string() | binary(), Out::string() | binary(), Code::term()}, Data::term()) -&gt; term()), Data::term()} | fun(({ok, In::string() | binary(), Out::string() | binary()} | {error, In::string() | binary(), Out::string() | binary(), Code::term()}) -&gt; term()) | fun(() -&gt; term()) | sync | undefined) -&gt; {async, term()} | {ok, In::binary() | string(), Out::binary() | string()} | {error, term()}
</code></pre>
<br />

Convert a media

<a name="info-2"></a>

### info/2 ###

<pre><code>
info(File::<a href="file.md#type-filename_all">file:filename_all()</a>, Info::atom()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

Equivalent to [`info(File, Info, [])`](#info-3).

<a name="info-3"></a>

### info/3 ###

<pre><code>
info(File::<a href="file.md#type-filename_all">file:filename_all()</a>, Info::atom(), Options::<a href="#type-info_options">info_options()</a>) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

Return the given media information

<a name="infos-1"></a>

### infos/1 ###

<pre><code>
infos(File::<a href="file.md#type-filename_all">file:filename_all()</a>) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

Equivalent to [`infos(File, [])`](#infos-2).

<a name="infos-2"></a>

### infos/2 ###

<pre><code>
infos(File::<a href="file.md#type-filename_all">file:filename_all()</a>, Options::<a href="#type-info_options">info_options()</a>) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

Return all media informations

<a name="screenshot-2"></a>

### screenshot/2 ###

<pre><code>
screenshot(Movie::binary() | string(), Out::binary() | string()) -&gt; {ok, Movie::binary() | string(), Out::binary() | string()} | {error, term()}
</code></pre>
<br />

Create a screenshot for a movie

<a name="start-0"></a>

### start/0 ###

<pre><code>
start() -&gt; {ok, [atom()]} | {error, term()}
</code></pre>
<br />

Start vice application

<a name="status-1"></a>

### status/1 ###

<pre><code>
status(Worker::reference()) -&gt; running | {running, float()} | done
</code></pre>
<br />

Return the conversion status

<a name="stop-0"></a>

### stop/0 ###

<pre><code>
stop() -&gt; ok | {error, term()}
</code></pre>
<br />

Stop vice application

<a name="stop-1"></a>

### stop/1 ###

<pre><code>
stop(Worker::reference()) -&gt; ok | {error, term()}
</code></pre>
<br />

Stop a running job

<a name="thumbnails-2"></a>

### thumbnails/2 ###

`thumbnails(Movie, OutName) -> any()`

__This function is deprecated:__ use vice_thumbnails:generate/2

<a name="thumbnails-3"></a>

### thumbnails/3 ###

`thumbnails(Movie, OutName, Options) -> any()`

__This function is deprecated:__ use vice_thumbnails:thumbnails/3

<a name="to_html5_mp4-2"></a>

### to_html5_mp4/2 ###

`to_html5_mp4(Input, Output) -> any()`

__This function is deprecated:__ use convert/4 with `html5mp4` preset

<a name="to_html5_mp4-3"></a>

### to_html5_mp4/3 ###

`to_html5_mp4(Input, Output, Fun) -> any()`

__This function is deprecated:__ use convert/4 with `html5mp4` preset

<a name="to_html5_ogg-2"></a>

### to_html5_ogg/2 ###

`to_html5_ogg(Input, Output) -> any()`

__This function is deprecated:__ use convert/4 with `html5ogg` preset

<a name="to_html5_ogg-3"></a>

### to_html5_ogg/3 ###

`to_html5_ogg(Input, Output, Fun) -> any()`

__This function is deprecated:__ use convert/4 with `html5ogg` preset

<a name="to_html5_webm-2"></a>

### to_html5_webm/2 ###

`to_html5_webm(Input, Output) -> any()`

__This function is deprecated:__ use convert/4 with `html5webm` preset

<a name="to_html5_webm-3"></a>

### to_html5_webm/3 ###

`to_html5_webm(Input, Output, Fun) -> any()`

__This function is deprecated:__ use convert/4 with `html5webm` preset

<a name="type-1"></a>

### type/1 ###

<pre><code>
type(File::<a href="file.md#type-filename_all">file:filename_all()</a>) -&gt; audio | video | image | unknow
</code></pre>
<br />

Return the file type (image, audio or video)

<a name="webvtt-2"></a>

### webvtt/2 ###

`webvtt(Movie, OutName) -> any()`

__This function is deprecated:__ use vice_thumbnails:generate/2

<a name="webvtt-3"></a>

### webvtt/3 ###

`webvtt(Movie, OutName, Options) -> any()`

__This function is deprecated:__ use vice_thumbnails:generate/3

