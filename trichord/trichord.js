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
var trichord = new Array(6);

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
  deleteObservers();

  push     = new Push(this);
  ourTrack = new OurTrack(push, push.controlButtonMatrix);

  // Colors to change the first trichord
  push.setColor(0, 2, 65);
  push.setColor(0, 3, 69);
  push.setColor(0, 4, 4);
  push.setColor(0, 5, 2);

  // Colors to change the second trichord
  push.setColor(7, 2, 73);
  push.setColor(7, 3, 77);
  push.setColor(7, 4, 12);
  push.setColor(7, 5, 10);

  // Colors for the notes (default to miyako-bushi)
  push.setColor(1, 4, 64); // C
  push.setColor(2, 4, 65); // C#/D/D#/E
  push.setColor(3, 4, 64); // F
  push.setColor(4, 4, 64); // G
  push.setColor(5, 4, 73); // G#/A/A#/B
  push.setColor(6, 4, 64); // C

  // Actions to change the first trichord
  push.setAction(0, 2, updateScale(2, 4, 1, 49));
  push.setAction(0, 3, updateScale(2, 4, 1, 50));
  push.setAction(0, 4, updateScale(2, 4, 1, 51));
  push.setAction(0, 5, updateScale(2, 4, 1, 52));

  // Actions to change the second trichord
  push.setAction(7, 2, updateScale(5, 4, 4, 56));
  push.setAction(7, 3, updateScale(5, 4, 4, 57));
  push.setAction(7, 4, updateScale(5, 4, 4, 58));
  push.setAction(7, 5, updateScale(5, 4, 4, 59));

  // Actions to play notes
  push.setAction(1, 4, sendNote(0));
  push.setAction(2, 4, sendNote(1));
  push.setAction(3, 4, sendNote(2));
  push.setAction(4, 4, sendNote(3));
  push.setAction(5, 4, sendNote(4));
  push.setAction(6, 4, sendNote(5));

  // The fixed notes of the trichord
  trichord[0] = 48;
  trichord[2] = 53;
  trichord[3] = 55;
  trichord[5] = 60;

  // Default to the miyako-bushi scale
  trichord[1] = 49;
  trichord[4] = 56;
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

function deleteObservers() {
  if(ourTrack != null) ourTrack.deleteObservers();
  if(push     != null) push.deleteObservers();
}

/*******************************************************************************
  Internal functions
*******************************************************************************/

/**
 * Update part of the scale
 */
function updateScale(scaleCol, scaleRow, scaleIndex, newNote) {
  return function(pressedCol, pressedRow, color, velocity) {
    // Update on button release
    if(velocity == 0) {
      push.setColor(scaleCol, scaleRow, color);
      trichord[scaleIndex] = newNote;
    }
  }
}

/**
 * Send the played to outlet 1
 */
function sendNote(note) {
  return function(col, row, color, velocity) {
    outlet(0, [trichord[note], velocity]);
  }
}
sendNote.local = 1;
