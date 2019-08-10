"use strict";

if (typeof navigator.mediaDevices.getUserMedia !== 'function') {
    const err = new Error('getUserMedia()が利用できないブラウザです！');
    alert(`${err.name} ${err.message}`);
    throw err;
}

const flags = {
  ids: { video: 'video_area', capture: 'capture_image' },
  size: { width: 400, height: 300 }
}

const app = Elm.Main.init(
  { node: document.getElementById('main')
  , flags: flags
  }
);

const video = document.getElementById(flags.ids.video);
app.ports.startCamera.subscribe(function() {
  navigator.mediaDevices.getUserMedia({ video: flags.size, audio: false })
  .then(stream => video.srcObject = stream)
  .catch(err => alert(`${err.name} ${err.message}`));
})

app.ports.captureImage.subscribe(function() {
  var canvas_capture_image = document.getElementById(flags.ids.capture);
  var cci = canvas_capture_image.getContext('2d');
  var va = document.getElementById(flags.ids.video);
  canvas_capture_image.width  = va.videoWidth;
  canvas_capture_image.height = va.videoHeight;
  cci.drawImage(va, 0, 0);

  var image = cci.getImageData(0, 0, va.videoWidth, va.videoHeight);
  // Send image data as Array because sending non array object is very heavy.
  app.ports.updateImage.send(
    { data: Array.from(image.data)
    , width: image.width
    , height: image.height
    }
  );
})
