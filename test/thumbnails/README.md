## Prepare

```
make dev

1> vice:start().
2> vice:thumbnails("test/thumbnails/big_buck_bunny.mp4", "webvtttest", [{out_path, "test/thumbnails"}, {assets_path, ""}]).
```

## Test

```
cd  test/thumbnails
python -m SimpleHTTPServer
```
