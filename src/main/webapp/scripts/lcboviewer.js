"use strict";

// Global variable, singleton lcboViewer
// API:
// <localhost:port>/store/lat/<lat>/lon/<lon> gives closest store from coordinate(lat,lon).
// Currently supported Store  (see lcboapi.com): id, name, is_dead, latitude, longitude, distance_in_meters (from specified location), address_line_1, city
var lcboViewer = (function() {
    var defaultOntarioLocation = new google.maps.LatLng(43.647219, -79.3789987); // Bay & Front LCBO address.
    var mapCanvas = document.getElementById('map-canvas');
    var map;
    var userMarker;
    var closestStoreName = 'Unknown Store';
    var fetchStore = function(userLatLng, userLocationAvailable, addMarker) {
        var myOptions = {
            center:userLatLng,
            zoom:11,
            mapTypeId:google.maps.MapTypeId.HYBRID,
            mapTypeControl:true
        }
        if (addMarker == true) {
            map = new google.maps.Map(mapCanvas, myOptions);
            userMarker = new google.maps.Marker({position:userLatLng,map:map,title:"Current Location",icon:"http://maps.google.com/mapfiles/ms/icons/green-dot.png"});
        }
        // Show particulars of nearby store in storeNearby and also show that point in a google map.
        $.ajax({
            // liftweb interprets periods in special way for suffixes in JSON calls (req parsing), so we escape them by using commas instead.
            url: '/stores/lat/' + userLatLng.lat().toString().replace('.',  ',') + '/lng/' + userLatLng.lng().toString().replace('.', ','),
            type: 'GET',
            success: function(data, status){
                var latlng = new google.maps.LatLng(data.latitude, data.longitude)
                var title = 'Downtown Toronto Liquor Store';
                if (userLocationAvailable) {
                    var distOutput = (data.distance_in_meters/1000.0).toFixed(2) + ' kms';
                    $("#storeNearby").html('Selected LCBO store:');
                    title = "Selected Store";
                    $("#storeName").html(data.name);
                    closestStoreName = data.name;
                    $("#storeAddressLine1").html(data.address_line_1);
                    $("#storeCity").html(data.city);
                    $("#storeDistance").html(distOutput);
                    $("#storeLat").html(data.latitude);
                    $("#storeLon").html(data.longitude);
                    $("#storeAttributesTbl").show();
                } else {
                    $("#storeNearby").html("Downtown Toronto LCBO (user location unavailable)");
                    $("#storeAttributesTbl").hide();
                }
                if (addMarker == true) {
                    var closestMarker = new google.maps.Marker({position:latlng,map:map,title:title,icon:"http://maps.google.com/mapfiles/ms/icons/blue-dot.png"});
                    lcboViewer.fetchStores();
                }
            },
            error: function(data, status){
                console.log("Error Data: " + data.responseText + "\nStatus: " + status );
                alert(data.responseText );
            }
        });
    };

    var fetchStoreNearUser = function(position) {
        fetchStore(new google.maps.LatLng(position.coords.latitude, position.coords.longitude),
            true,
            true);
    };

    var showGeoError = function(error) {
         fetchStore(defaultOntarioLocation, false, true);
    };

    return {
        fetchStores: function() {
                var viewSouthWest = map.getBounds().getSouthWest();
                var viewNorthEast = map.getBounds().getNorthEast();
                var url = '/stores/swlat/' + viewSouthWest.lat().toString().replace('.',  ',') + '/swlng/' + viewSouthWest.lng().toString().replace('.', ',')
                                      + '/nelat/' + viewNorthEast.lat().toString().replace('.', ',')+ '/nelng/' + viewNorthEast.lng().toString().replace('.', ',');
                $.ajax({
                    url: url,
                    type: 'GET',
                    success: function(data, status){
                        function createMarker(element, index, array) {
                            if (element.name != closestStoreName) {
                                var latlng = new google.maps.LatLng(element.latitude, element.longitude);
                                var marker = new google.maps.Marker({position:latlng,map:map});
                                marker.addListener('click', function(e){
                                    fetchStore(e.latLng, true, false);
                                });
                            }
                        }
                        data.forEach(createMarker);
                    },
                    error: function(data, status){
                        console.log("Error Data: " + data.responseText + "\nStatus: " + status );
                        alert(data.responseText );
                    }
                });
        },

        fetchStoreFromPosition: function() {
            if (navigator.geolocation) {
                navigator.geolocation.getCurrentPosition(fetchStoreNearUser, showGeoError);
            } else  {
                $("#storeNearby").html("Geolocation is not supported by this browser.");
                fetchStore(defaultOntarioLocation, false, true);
            }
        }
    }
}());

$(document).ready(function(){
    lcboViewer.fetchStoreFromPosition();
});


