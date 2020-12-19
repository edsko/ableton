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

var push     = null;
var ourTrack = null;

/*******************************************************************************
  Handle M4L messages
*******************************************************************************/

// TODO: Introduce PushInstrument class
// Then make Trichord a derived class.

/**
 * Initialize the device.
 *
 * Should be called when the M4L device is fully loaded (use 'live.thisdevice').
 *
 * NOTE: This will not work when looking at the 'trichord' patcher (that is,
 * 'trichord.maxpat'); can only be used inside the scope of the 'trichord'
 * M4L device ('trichord.amxd').
 */
function init() {
  if(push     != null) push.deleteCallbacks();
  if(ourTrack != null) ourTrack.deleteCallbacks();

  push     = new Push(buttonPressed);
  ourTrack = new OurTrack(push, push.controlButtonMatrix);

  push.setColor(0, 2, 65);
  push.setColor(0, 3, 69);
  push.setColor(0, 4, 4);
  push.setColor(0, 5, 2);

  push.setColor(7, 2, 65 + 8);
  push.setColor(7, 3, 69 + 8);
  push.setColor(7, 4, 4  + 8);
  push.setColor(7, 5, 2  + 8);

  push.setColor(1, 4, 64); // C
  push.setColor(2, 4, 64); // C#/D/D#/E
  push.setColor(3, 4, 64); // F
  push.setColor(4, 4, 64); // G
  push.setColor(5, 4, 64); // G#/A/A#/B
  push.setColor(6, 4, 64); // C
}

function loadbang() {
  post("this is loadbang!\n");
}

// TODO:
// https://cycling74.com/forums/does-anyone-know-when-these-messages-occur
// Talks about loadbang versus live.thisdevice, as well as closebang/savebang

function grab() {
  push.grab();
}

function release() {
  push.release();
}

function showColors(page) {
  push.showColors(page);
}

/*******************************************************************************
  Private functions
*******************************************************************************/

function buttonPressed(args) {
  // push.setColor(args.col, args.row, 10);
}
buttonPressed.local = 1;
