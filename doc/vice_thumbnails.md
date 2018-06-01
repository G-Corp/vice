

# Module vice_thumbnails #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#generate-2">generate/2</a></td><td>Equivalent to <a href="#generate-4"><tt>generate(Movie, OutName,
[{every, 1}, {width, 100}, {out_path, "."},
{sprite, true}, {assets_path, ""}],
undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#generate-3">generate/3</a></td><td>Equivalent to <a href="#generate-4"><tt>generate(Movie, OutName, Options, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#generate-4">generate/4</a></td><td> 
Generate a video thumbnails (.vtt + sprite).</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="generate-2"></a>

### generate/2 ###

`generate(Movie, OutName) -> any()`

Equivalent to [`generate(Movie, OutName,[{every, 1}, {width, 100}, {out_path, "."},{sprite, true}, {assets_path, ""}],undefined)`](#generate-4).

<a name="generate-3"></a>

### generate/3 ###

`generate(Movie, OutName, Options) -> any()`

Equivalent to [`generate(Movie, OutName, Options, undefined)`](#generate-4).

<a name="generate-4"></a>

### generate/4 ###

<pre><code>
generate(Movie::binary() | string(), OutName::binary() | string(), Options::list(), Fun::{fun((Data::term()) -&gt; term()) | fun(({ok, In::string() | binary(), Out::string() | binary()} | {error, In::string() | binary(), Out::string() | binary(), Code::term()}, Data::term()) -&gt; term()), Data::term()} | fun(({ok, In::string() | binary(), Out::string() | binary()} | {error, In::string() | binary(), Out::string() | binary(), Code::term()}) -&gt; term()) | fun(() -&gt; term()) | sync | undefined) -&gt; {async, reference()} | {ok, Out::binary() | string()} | {error, term()}
</code></pre>
<br />


Generate a video thumbnails (.vtt + sprite)

Options:

* `every :: integer()`

* `width :: integer()`

* `out_path :: string()`

* `assets_path :: string()`

* `sprite :: true | false`


