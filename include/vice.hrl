-define(VIDEO_ENCODERS, [vice_prv_ffmpeg]).
-define(PHOTO_ENCODERS, [vice_prv_imagemagick]).
-define(AUDIO_ENCODERS, [vice_prv_sox]).
-define(DEFAULT_ENCODERS, #{
          video => [vice_prv_ffmpeg],
          image => [vice_prv_imagemagick],
          audio => [vice_prv_sox]
         }).
