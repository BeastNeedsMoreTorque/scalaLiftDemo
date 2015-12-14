"use strict";
// Global variable, singleton lcboViewer
var lcboViewer = (function() {

  return {
    showPosition: function(position) {
        // Show coordinates now, later on a map centered at position
        $("#geolocation").html('ALMOST THERE!! You are at Latitude:' + position.coords.latitude + ' Longitude:' + position.coords.longitude + ' where is the next LCBO?');
        $("#geolocationLAT").html(position.coords.latitude);
        $("#geolocationLON").html(position.coords.longitude);
    }
  }
}());


