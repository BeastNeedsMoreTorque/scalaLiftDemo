"use strict";
// Global variable, singleton lcboViewer
var lcboViewer = (function() {

  var currPosition = null;
  return {
    sendPosition: function() {
        if (typeof currPosition != 'undefined' && currPosition != null ) {
          console.log(currPosition);
          var geoJS = JSON.stringify({lat: currPosition.latitude, lon: currPosition.longitude});
          console.log(geoJS);
          //$.ajax({url: '/geo/lat/' + currPosition.latitude + '/lon/' +currPosition.longitude,
          $.ajax({url: '/lat/' + currPosition.latitude + '/lon/' +currPosition.longitude,
                 type: 'GET',
                 success: function(data, status){
                   //console.log("Success Data: " + data );
                   console.log("status: " + status );

                 },
                 error: function(data, status){
                   console.log("Error Data: " + data + "\nStatus: " + status );
                 }
          });
        }
    },

    showPosition: function(position) {
        // Show coordinates now, later on a map centered at position
        if (typeof position != 'undefined') currPosition = position.coords;
        $("#geolocation").html('ALMOST THERE!! You are at Latitude:' + position.coords.latitude + ' Longitude:' + position.coords.longitude + ' where is the next LCBO?');
        $("#geolocationLAT").html(position.coords.latitude);
        $("#geolocationLON").html(position.coords.longitude);
    }

  }
}());


