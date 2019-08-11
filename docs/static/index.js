"use strict";

if (typeof navigator.mediaDevices.getUserMedia !== 'function') {
    const err = new Error('Your browser can not use getUserMedia().');
    alert(`${err.name} ${err.message}`);
    throw err;
}

const flags = {
  ids: { video: 'video_area', capture: 'capture_image' },
  size: { width: 300, height: 300 }
};

const app = Elm.Main.init(
  { node: document.getElementById('main')
  , flags: flags
  }
);

const constraints = {
  audio: false,
  video: {...flags.size, facingMode: "environment" }
};

function handleSuccess(stream) {
  const video = document.getElementById(flags.ids.video);
  const videoTracks = stream.getVideoTracks();
  console.log('Got stream with constraints:', constraints);
  console.log(`Using video device: ${videoTracks[0].label}`);
  window.stream = stream; // make variable available to browser console
  video.srcObject = stream;
}

function handleError(error) {
  if (error.name === 'ConstraintNotSatisfiedError') {
    let v = constraints.video;
    alert(`The resolution ${v.width.exact}x${v.height.exact} px is not supported by your device.`);
  } else if (error.name === 'PermissionDeniedError') {
    alert('Permissions have not been granted to use your camera and ' +
      'microphone, you need to allow the page access to your devices in ' +
      'order for the demo to work.');
  }
  alert(`getUserMedia error: ${error.name}`);
  console.error(error);
}

async function initCamera() {
  try {
    const stream = await navigator.mediaDevices.getUserMedia(constraints);
    handleSuccess(stream);
  } catch (e) {
    handleError(e);
  }
}

const video = document.getElementById(flags.ids.video);
app.ports.startCamera.subscribe(function() { initCamera() });

app.ports.captureImage.subscribe(function() {
  var canvas_capture_image = document.getElementById(flags.ids.capture);
  var cci = canvas_capture_image.getContext('2d');
  var va = document.getElementById(flags.ids.video);
  canvas_capture_image.width  = va.videoWidth;
  canvas_capture_image.height = va.videoHeight;
  cci.drawImage(va, 0, 0);

  var image = cci.getImageData(0, 0, va.videoWidth, va.videoHeight);
  var qrcode = jsQR(image.data, image.width, image.height)
  app.ports.updateQRCode.send(qrcode);
});
