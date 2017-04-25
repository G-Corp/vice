(function() {
  var defaults = {
      width:0, height:0, basePath : ""
    },
    extend = function() {
      var args, target, i, object, property;
      args = Array.prototype.slice.call(arguments);
      target = args.shift() || {};
      for (i in args) {
        object = args[i];
        for (property in object) {
          if (object.hasOwnProperty(property)) {
            if (typeof object[property] === 'object') {
              target[property] = extend(target[property], object[property]);
            } else {
              target[property] = object[property];
            }
          }
        }
      }
      return target;
    },
    getComputedStyle = function(el, pseudo) {
      return function(prop) {
        if (window.getComputedStyle) {
          return window.getComputedStyle(el, pseudo)[prop];
        } else {
          return el.currentStyle[prop];
        }
      };
    },
    offsetParent = function(el) {
      if (el.nodeName !== 'HTML' && getComputedStyle(el)('position') === 'static') {
        return offsetParent(el.offsetParent);
      }
      return el;
    },
    getScrollOffset = function() {
      if (window.pageXOffset) {
        return {
          x: window.pageXOffset,
          y: window.pageYOffset
        };
      }
      return {
        x: document.documentElement.scrollLeft,
        y: document.documentElement.scrollTop
      };
    },
    parseImageLink = function(imglocation) {
      var lsrc, clip, hashindex, hashstring;
      hashindex = imglocation.indexOf('#');
      if (hashindex === -1) {
        return {src:imglocation,w:0,h:0,x:0,y:0};
      }
      lsrc = imglocation.substring(0,hashindex);
      hashstring = imglocation.substring(hashindex+1);
      if (hashstring.substring(0,5) !== 'xywh=') {
        return {src:defaults.basePath + lsrc,w:0,h:0,x:0,y:0};
      }
      var data = hashstring.substring(5).split(',');
      return {src:defaults.basePath + lsrc,w:parseInt(data[2]),h:parseInt(data[3]),x:parseInt(data[0]),y:parseInt(data[1])};
    };

  /**
   * register the thubmnails plugin
   */
  videojs.plugin('thumbnails', function(options) {
    var div, settings, img, player, progressControl, duration, moveListener, moveCancel, thumbTrack;
    settings = extend({}, defaults, options);
    player = this;

    if (!options) {
      return;
    }
    var player = this;
    if (!options.vtt) {
      return;
    }
    defaults.basePath = options.basePath || options.vtt.substring(0, options.vtt.lastIndexOf('/') + 1);;
    console.log(defaults.basePath);

    // when the container is MP4
    player.on('durationchange', function(event) {
      duration = player.duration();
    });

    moveListener = function(event) {
      var mouseTime, time, active, left, setting, pageX, right, width, halfWidth, pageXOffset, clientRect;
      active = 0;
      mouseTime = player.controlBar.progressControl.seekBar.calculateDistance(event) * duration;

      //Now check which of the cues applies
      var cnum = thumbTrack && thumbTrack.cues.length;
      i = 0;
      while (i<cnum) {
        var ccue = thumbTrack.cues[i];
        if (ccue.startTime <= mouseTime && ccue.endTime >= mouseTime) {
          setting = parseImageLink(ccue.text);
          break;
        }
        i++;
      }
      //None found, so show nothing
      if (typeof setting === 'undefined') {
        return;
      }

      //Changed image?
      if (setting.src && img.src != setting.src) {
        img.src = setting.src;
      }

      //Fall back to plugin defaults in case no height/width is specified
      if (setting.w === 0) {
        setting.w = settings.width;
      }
      if (setting.h === 0) {
        setting.h = settings.height;
      }

      //Set the container width/height if it changed
      var padding = 2;
      var textwidth = 60;

      var compareh = (setting.h + padding*2) + 'px';
      var comparew = (setting.w + padding*2) + 'px';
      if (div.style.width != comparew || div.style.height != compareh) {
        div.style.width = (setting.w + padding*2) + 'px';
        div.style.height = (setting.h + padding*2) + 'px';
        div.style.top = -(setting.h + 15) + 'px';
	if (padding > 0) {
          div.style.border = padding + 'px solid rgba(0,0,0,0.9)';
	}
	//div.style.position = 'relative';
        span.style.width = textwidth + 'px';
        span.style.left = (setting.w/2 - textwidth/2) + 'px';
      }
      //Set the image cropping
      img.style.position = 'absolute';
      img.style.left = -(setting.x) + 'px';
      img.style.top  = -(setting.y) + 'px';
      img.style.clip = 'rect('+setting.y+'px,'+(setting.w+setting.x)+'px,'+(setting.y+setting.h)+'px,'+setting.x+'px)';
      console.log(img);

      var timeStr = new Date(1000 * mouseTime).toISOString().substr(11, 8);
      span.innerHTML = timeStr;

      width = setting.w;
      halfWidth = width / 2;

/*      // make sure that the thumbnail doesn't fall off the right side of the left side of the player
      if ( (left + halfWidth) > right ) {
        left = right - width;
      } else if (left < halfWidth) {
        left = 0;
      } else {
        left = left - halfWidth;
      }
*/
      div.style.left = -(halfWidth) + 'px';

    };

    moveCancel = function(event) {
      //console.log(event);
      if (event.explicitOriginalTarget) {
          console.log(event.explicitOriginalTarget.className); }

      if ((! event.explicitOriginalTarget) ||
          ((event.explicitOriginalTarget.ClassName !== 'vjs-mouse-display') &&
          (event.explicitOriginalTarget.ClassName !== 'vjs-progress-holder vjs-slider vjs-slider-horizontal')))  {
      console.log("done");
      div.style.left = '-1000px'; }
    };

    player.ready(function() {

    var trackEl = player.addRemoteTextTrack({
      id: 'thumbnails',
      kind: 'metadata',
      src: options.vtt,
    });
    trackEl.addEventListener('load', function onLoad() {
      trackEl.removeEventListener('load', onLoad);
      thumbTrack = player.textTracks().getTrackById('thumbnails');
    });

    (function() {
      var progressControl, addFakeActive, removeFakeActive;
      // Android doesn't support :active and :hover on non-anchor and non-button elements
      // so, we need to fake the :active selector for thumbnails to show up.
      if (navigator.userAgent.toLowerCase().indexOf("android") !== -1) {
        progressControl = player.controlBar.progressControl;

        addFakeActive = function() {
          progressControl.addClass('fake-active');
        };
        removeFakeActive = function() {
          progressControl.removeClass('fake-active');
        };

//        progressControl.on('touchstart', addFakeActive);
//        progressControl.on('touchend', removeFakeActive);
//        progressControl.on('touchcancel', removeFakeActive);
      }
    })();

    // create the thumbnail
    div = document.createElement('div');
    div.className = 'vjs-thumbnail-holder';
    img = document.createElement('img');
    div.appendChild(img);
    img.className = 'vjs-thumbnail';
    span = document.createElement('span');
    div.appendChild(span);
    span.className = 'vjs-thumbnail-text';

    // keep track of the duration to calculate correct thumbnail to display
    duration = player.duration();

    // add the thumbnail to the player
    progressControl = player.controlBar.progressControl;
    //progressControl.el().appendChild(div);

    MTD = player.controlBar.progressControl.seekBar.mouseTimeDisplay;
    MTD.el().appendChild(div);
    //MTD.on('mousemove', MTD_moveListener);

    // update the thumbnail while hovering
    progressControl.on('mousemove', moveListener);
    progressControl.on('touchmove', moveListener);

    // move the placeholder out of the way when not hovering
/*    progressControl.on('mouseout', moveCancel);
*/    progressControl.on('touchcancel', moveCancel);
    progressControl.on('touchend', moveCancel);
    player.on('userinactive', moveCancel);

    }, true);

  });
})();
