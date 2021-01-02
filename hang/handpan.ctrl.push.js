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

inlets    = 2;
outlets   = 2;
autowatch = 0;

/*******************************************************************************
  Imports
*******************************************************************************/

var Push     = require("push").Push;
var OurTrack = require("ourtrack").OurTrack;
var State    = require("trichordstate").TrichordState;
var Scale    = require("trichordstate").Scale;

/*******************************************************************************
  Global variables
*******************************************************************************/

var push        = null;
var ourTrack    = null;
var activeScale = null;
var state       = new State();

/*******************************************************************************
  Constants
*******************************************************************************/

// For each trichord, the color of the middle note per scale
var trichordColors = [
    [65, 69, 4, 2]
  , [73, 77, 12, 10]
  ];

/*******************************************************************************
  Handle M4L messages
*******************************************************************************/

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

    push.setAction(0, 2 + i, setCustom(0, i));
    push.setAction(7, 2 + i, setCustom(1, i));
  }

  // Set up buttons to play notes
  for(var i = 0; i < 6; i++) {
    push.setColor(1 + i, 4, 64);
    push.setAction(1 + i, 4, sendNote(i));
  }

  // Update the push with our current state
  // The state will have been updated in response to the messages from
  // 'live.dial' or 'pattr' before 'live.thisdevice' calls 'init'.
  updatePush();
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
 * Dispatch other messages
 */
function anything() {
  switch(messagename) {
    // Messages that update the state
    case 'scale':
    case 'root':
    case 'custom1':
    case 'custom2':
      state[messagename] = arguments[0];
      updatePush();
      break;

    // Messages we just forward directly to the push object
    case 'showColors':
    case 'controlButtonMatrix':
      if(push != null) {
        push[messagename].apply(push, arguments);
      }
      break;

    default:
      error("Message '" + messagename + "' not understood\n");
      break;
  }
}

/*******************************************************************************
  Internal functions
*******************************************************************************/

/**
 * Update the Push to reflect our internal state
 *
 * @private
 */
function updatePush() {
  // Cache the scale (so that we can respond to button presses)
  activeScale = state.getScale();

  if (push != null) {
    // Update the colors of the button matrix
    push.setColor(2, 4, trichordColors[0][state.getTrichord1()]);
    push.setColor(5, 4, trichordColors[1][state.getTrichord2()]);
  }
}
updatePush.local = 1;

/**
 * Respond to user selecting a custom scale
 *
 * @private
 */
function setCustom(trichord, offset) {
  return function(col, row, color, velocity) {
    if(velocity == 0) {
      // Override the scale, both in our state and in the UI
      state.scale = Scale.CUSTOM;
      outlet(1, ["scale", state.scale]);

      // Update the scale (and send it out to the pattr for storage)
      switch(trichord) {
        case 0:
          state.custom1 = offset;
          outlet(1, ["custom1", offset]);
          break;
        case 1:
          state.custom2 = offset;
          outlet(1, ["custom2", offset]);
          break;
      }

      updatePush();
    }
  }
}
setCustom.local = 1;

/**
 * Send the played note to outlet 1
 *
 * @private
 */
function sendNote(note) {
  return function(col, row, color, velocity) {
    outlet(0, [48 + activeScale[note], velocity]);
  }
}
sendNote.local = 1;
