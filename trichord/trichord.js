/**
 * Custom Push2 instrument: Trichords
 * This code is intended as a tutorial, not for production usage.
 *
 * @module trichord
 * @description Top-level module, intended for use with M4L 'js' object.
 * @author Edsko de Vries <edsko@edsko.net>
 * @copyright Edsko de Vries, 2020
 */

/*******************************************************************************
  Device initialization

  Disable automatch because before the device is disabled, the 'deleteObservers'
  message should be sent.
*******************************************************************************/

inlets    = 3;
outlets   = 3;
autowatch = 0;

setinletassist(1, "From 'scale' dial");
setinletassist(2, "From 'root' dial");

setoutletassist(1, "To 'scale' dial");
setoutletassist(2, "To 'root' dial");

/*******************************************************************************
  Imports
*******************************************************************************/

var Push     = require("push").Push;
var OurTrack = require("ourtrack").OurTrack;

/*******************************************************************************
  Global variables
*******************************************************************************/

var push        = null;
var ourTrack    = null;
var activeScale = [48, 0, 53, 55, 0, 60];
var activeRoot  = 0;

/*******************************************************************************
  Constants
*******************************************************************************/

var scales = {
    RYU_KYU: 0
  , MIN_YO: 1
  , RITSU: 2
  , MIYAKO_BUSHI: 3
  , CUSTOM: 4
  };

// For each trichord, the choice for the middle note per scale
var trichordNotes = [
    [49, 50, 51, 52]
  , [56, 57, 58, 59]
  ];

// For each trichord, the color of the middle note per scale
var trichordColors = [
    [65, 69, 4, 2]
  , [73, 77, 12, 10]
  ];

/*******************************************************************************
  Handle M4L messages
*******************************************************************************/

// TODO: Introduce PushInstrument class
// Then make Trichord a derived class.

/**
 * Initialize the device.
 *
 * Should be called when the M4L device is fully loaded
 * (use <code>live.thisdevice</code>).
 */
function init() {
  deleteObservers();

  push     = new Push(this);
  ourTrack = new OurTrack(this, function(trackNo, selected) {
    push.controlButtonMatrix(selected);
  });

  // Set up buttons to change the scales
  for(var i = 0; i < 4; i++) {
    push.setColor(0, 2 + i, trichordColors[0][i]);
    push.setColor(7, 2 + i, trichordColors[1][i]);

    push.setAction(0, 2 + i, updateScale(0, i));
    push.setAction(7, 2 + i, updateScale(1, i));
  }

  // Set up buttons to play notes
  for(var i = 0; i < 6; i++) {
    push.setColor(1 + i, 4, 64);
    push.setAction(1 + i, 4, sendNote(i));
  }

  // Set up defaults
  outlet(1, scales.RYU_KYU);
  outlet(2, 0);
  setScale(scales.RYU_KYU);
}

/**
 * Show all available colors on the push
 *
 * @param {number} page Which page of colors (0 or 1)
 */
function showColors(page) {
  push.showColors(page);
}

/**
 * Delete all observers.
 *
 * Should be called when the device is reloaded or removed.
 */
function deleteObservers() {
  if(ourTrack != null) ourTrack.deleteObservers();
  if(push     != null) push.deleteObservers();
}

/**
 * Respond to <code>int</code> messages on inlet 1 (scale) or 2 (root).
 *
 * @param {number} i The incoming <code>int</code>
 */
function msg_int(i) {
  switch(inlet) {
    case 1:
      setScale(i);
      break;
    case 2:
      setRoot(i);
      break;
  }
}

/**
 * Respond to the device being enabled or disabled.
 *
 * If the device is disabled, we relinguish control over the button matrix.
 *
 * @param {boolean} enabled <code>true</code> if the device is enabled
 */
function setEnabled(enabled) {
  if(push == null) return;
  push.controlButtonMatrix(enabled);
}

function scheduleRelease() {
  post("scheduleRelease\n");

  var context = {};

  var task = new Task(function() {
    var push = new Push(this);
    push.controlButtonMatrix(false);
  }, context);
  task.schedule(0);
}

function notifydeleted() {
  scheduleRelease();
}

// TODO: read https://docs.cycling74.com/max8/tutorials/pattrchapter02

/*******************************************************************************
  Internal functions
*******************************************************************************/

/**
 * Update part of the scale
 *
 * @private
 */
function updateScale(trichord, scale) {
  return function(col, row, color, velocity) {
    if(velocity == 0) {
      updateTrichord(trichord, scale);
      outlet(1, scales.CUSTOM); // Set scale dial to custom
    }
  }
}
updateScale.local = 1;

/**
 * Send the played note to outlet 1
 *
 * @private
 */
function sendNote(note) {
  return function(col, row, color, velocity) {
    outlet(0, [activeScale[note] + activeRoot, velocity]);
  }
}
sendNote.local = 1;

/**
 * Change one of the two trichords
 *
 * @private
 */
function updateTrichord(trichord, scale) {
  switch(trichord) {
    case 0:
      activeScale[1] = trichordNotes[trichord][scale];
      push.setColor(2, 4, trichordColors[trichord][scale]);
      break;
    case 1:
      activeScale[4] = trichordNotes[trichord][scale];
      push.setColor(5, 4, trichordColors[trichord][scale]);
      break;
  }
}
updateTrichord.local = 1;

/**
 * Respond to scale changes
 *
 * @private
 */
function setScale(scale) {
  if(push  == null)          return;
  if(scale == scales.CUSTOM) return;

  updateTrichord(0, scale);
  updateTrichord(1, scale);
}
setScale.local = 1;

/**
 * Respond to root changes
 *
 * @private
 */
function setRoot(root) {
  activeRoot = root;
}
setRoot.local = 1;
