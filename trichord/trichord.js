/**
 * Custom Push2 instrument: Trichords
 * Written by Edsko de Vries <edsko@edsko.net>, 2020
 *
 * This code is intended as a tutorial, not for production usage.
 *
 * @module trichord
 * Top-level module, intended for use with M4L 'js' object.
 */

inlets  = 1;
outlets = 1;

/*******************************************************************************
  Imports
*******************************************************************************/

var Push     = require("push").Push;
var OurTrack = require("ourtrack").OurTrack;

/*******************************************************************************
  Global variables
*******************************************************************************/

var push     = new Push(buttonPressed);
var ourTrack = new OurTrack(ourTrackSelected);

/*******************************************************************************
  Handle M4L messages
*******************************************************************************/

/**
 * Find the Push controller
 *
 * Should be called when the M4L device is fully loaded (use 'live.thisdevice').
 *
 * NOTE: This will not work when looking at the 'trichord' patcher (that is,
 * 'trichord.maxpat'); can only be used inside the scope of the 'trichord'
 * M4L device ('trichord.amxd').
 */
function init() {
  push.init();
  if(ourTrack.isSelected()) push.grab();
}

/*******************************************************************************
  Private functions
*******************************************************************************/

function buttonPressed(args) {
  post("pressed", args.velocity, args.col, args.row);
  post();
}
buttonPressed.local = 1;

function ourTrackSelected(selected) {
  if(selected) {
    push.grab();
  } else {
    push.release();
  }
}
ourTrackSelected.local = 1;
