"use strict";
// Global variable, singleton lcboViewer
// API:
// <localhost:port>/store/lat/<lat>/lon/<lon> gives closest store from coordinate(lat,lon).
// Currently supported Store  (see lcboapi.com): id, name, is_dead, latitude, longitude, distance_in_meters (from specified location), address_line_1, city
var lcboViewer = (function() {
    var imgSelectStyle = 'imgframe';
    var fetchStore = function(position) {
        // Show particulars of nearby store in storeNearby and also show that point in a google map.
        $.ajax({
            // liftweb interprets periods in special way for suffixes in JSON calls (req parsing), so we escape them by using commas instead.
            url: '/store/lat/' + position.coords.latitude.toString().replace('.',  ',') + '/lon/' +position.coords.longitude.toString().replace('.', ','),
            type: 'GET',
            success: function(data, status){
                var latlon = new google.maps.LatLng(data.latitude, data.longitude)
                var mapholder = document.getElementById('mapholder')
                mapholder.style.height = '200px';
                mapholder.style.width = '350px';

                var myOptions = {
                    center:latlon,zoom:14,
                    mapTypeId:google.maps.MapTypeId.ROADMAP,
                    mapTypeControl:false,
                    navigationControlOptions:{style:google.maps.NavigationControlStyle.SMALL}
                }
                var map = new google.maps.Map(mapholder, myOptions);
                var marker = new google.maps.Marker({position:latlon,map:map,title:"Closest Liquor Store!"});

                // #storeNearby: You are XYZ.NN kms from branch <name> located at <address> in <city>
                var storeDesc = 'You are ' + (data.distance_in_meters/1000.0).toFixed(2) + ' kms from branch '+ data.name +
                    ' located at ' + data.address_line_1 + ' in ' + data.city;
                $("#storeNearby").html(storeDesc);
                },
            error: function(data, status){
                console.log("Error Data: " + data + "\nStatus: " + status );
                alert("Unable to locate nearest store from your location" );
            }
        });
     };

     var showGeoError = function(error) {
         switch(error.code) {
             case error.PERMISSION_DENIED:
                 $("#storeNearby").html("User denied the request for Geolocation.");
                 break;
             case error.POSITION_UNAVAILABLE:
                 $("#storeNearby").html("User Location information is unavailable.");
                 break;
             case error.TIMEOUT:
                 $("#storeNearby").html("The request to get user location timed out.");
                 break;
             case error.UNKNOWN_ERROR:
                 $("#storeNearby").html("An unknown error occurred.");
                 break;
         }
     };

    return {
        fetchStoreFromPosition: function() {
            if (navigator.geolocation) {
                navigator.geolocation.getCurrentPosition(fetchStore, showGeoError);
            } else  {
                $("#storeNearby").html("Geolocation is not supported by this browser.");
            }
        },

        frameRadioImage: function(container, elt) {
           var imgElts = $("#"+container).find("img").get();
           var i;
           for (i = 0; i < imgElts.length; i++) {
               if ($(imgElts[i]).attr('name') != elt) {
                   $(imgElts[i]).removeAttr('class');
               } else {
                   $(imgElts[i]).attr('class', imgSelectStyle);
               }
           }
        }
    }
}());


