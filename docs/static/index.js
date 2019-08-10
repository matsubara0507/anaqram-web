"use strict";

if (typeof navigator.mediaDevices.getUserMedia !== 'function') {
    const err = new Error('getUserMedia()が利用できないブラウザです！');
    alert(`${err.name} ${err.message}`);
    throw err;
}

const flags = { video: 'video_area', size: { width: 400, height: 300 } }

const app = Elm.Main.init(
  { node: document.getElementById('main')
  , flags: flags
  }
);

const video = document.getElementById(flags.video);
app.ports.startCamera.subscribe(function() {
  navigator.mediaDevices.getUserMedia({ video: flags.size, audio: false })
  .then(stream => video.srcObject = stream)
  .catch(err => alert(`${err.name} ${err.message}`));
})
