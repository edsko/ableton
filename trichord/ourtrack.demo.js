inlets  = 1;
outlets = 2;

var OurTrack = require("ourtrack").OurTrack;
var ourTrack = null;

function bang() {
  if(ourTrack == null) {
    ourTrack = new OurTrack(this, trackChanged);
  }
}

function trackChanged(trackNo, selected) {
  outlet(1, trackNo);
  outlet(0, selected);
}
