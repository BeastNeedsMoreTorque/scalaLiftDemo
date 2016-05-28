"use strict";

// ES 6 (2015) dependent for Map() (welcome to transpilers to address old non-compliant browsers, works fine on my Chrome on OS X)
// Global variable, singleton storeFinder
// API:
// <localhost:port>/stores gives us all stores with coordinates and we evaluate the closest.
// Uses Google maps API quite a bit.
var storeFinder = (function() {
  // Bay & Front LCBO address items.
  var defaultOntarioLocation;
  var DEFAULT_ON_LAT=43.647219;
  var DEFAULT_ON_LNG=-79.3789987;

  var KMS_PER_LAT = 111; // good approximation at 45' of latitude, which mostly works in Ontario (undefined if not near Ontario).
  var KMS_PER_LNG = 78.4;

  var GREEN_MARKER_ICON_URI = 'http://maps.google.com/mapfiles/ms/icons/green-dot.png';
  var BLUE_MARKER_ICON_URI = 'http://maps.google.com/mapfiles/ms/icons/blue-dot.png';
  var CURR_LOC_MARKER_TITLE = 'Current Location';
  var CLOSEST_STORE = 'Closest store';
  var SELECTED_STORE = 'Selected LCBO store:';
  var DEFAULT_STORE_LOCATION_ERR = 'Downtown Toronto LCBO (user location unavailable)';
  var GEO_LOC_UNAVAILABLE = 'Geolocation is not supported by this browser.';

  var ZOOM_LOWER_BOUND = 10;
  var STD_ZOOM = 12;

  var distMatrixService;
  var directionsService;
  var directionsDisplay;

  var mapCanvas = document.getElementById('map-canvas');
  var map;
  var markers = new Map(); // keyed by title (name of store + lcbo_id for uniqueness, otherwise if name only need an array here)
  var stores = [];
  var userMarker;
  var userLocation;
  var storeDistance;  // could also get storeDuration if we wanted.
  var mapOptions = {};

  var theSelectedStore = null;

  var buildKey = function(store) { // name is not unique! There are two pairs at King & Queen and on North 17!
    return store.name + ' , ' + store.lcbo_id;
  };

  var minDistanceStore = function (storesAndDists) { // FP storm is reaching JS, embrace it with use of reduce.
    if (storesAndDists.length == 0) return null;
    return storesAndDists.reduce( function(a, b) {
      if (a.dist < b.dist) return a;
      return b;
    },
    storesAndDists[0] );
  };

  var addUserDistances = function (stores, userLatLng) { // don't know if JS allocates and GCs as well as Scala, but let's assume so for now. FP map usage below.
    var withDistances = stores.map(function(s){
      var withDistance = {};
      var d = distanceByGeo(userLatLng, s.latitude, s.longitude);
      withDistance = {store:s, dist:d};
      return withDistance;
    });
    return withDistances;
  };

  // evaluate distance ourselves because of 25 destination distances limit in Google API, we have more than 600. Some hack.
  var distanceByGeo = function(latLng, refLat, refLng) {
    var x = KMS_PER_LAT * (latLng.lat()-refLat);  // reliable
    var y = KMS_PER_LNG * (latLng.lng()-refLng);  // approximately reliable at 45 North, which is typical Ontario (don't care about users out of continent).
    return (Math.sqrt(x*x + y*y));
  };

  var closestStore = function(latLng) {
    var bestDistance = +Infinity;
    var closest = null;
    var zippedStores = addUserDistances(stores, latLng); // collect user distances of each store with each store as intermediate step. Then find one with min distance
    closest = minDistanceStore(zippedStores).store;
    if (closest !== null && theSelectedStore == null) {
      flipMarker(closest);
    }
    return closest;
  };

  var flipMarker = function(store) {
    var marker = markers.get(buildKey(store));
    if (marker != undefined) {
      marker.setIcon(BLUE_MARKER_ICON_URI);
    }
  };

  var fetchStore = function(userLatLng) {
    theSelectedStore = closestStore(userLatLng);
    if (theSelectedStore !== null) {
      var closestLatLng = new google.maps.LatLng(theSelectedStore.latitude, theSelectedStore.longitude)
      evaluateDistance(closestLatLng);
      getDirections(closestLatLng);

      $('#storeNearby').html(SELECTED_STORE);
      $('#storeName').html(theSelectedStore.name);
      $('#storeAddressLine1').html(theSelectedStore.address_line_1);
      $('#storeCity').html(theSelectedStore.city);
      $('#storeLat').html(theSelectedStore.latitude);
      $('#storeLon').html(theSelectedStore.longitude);
      $('#storeAttributesTbl').show();
    } else {
      $('#storeNearby').html(DEFAULT_STORE_LOCATION_ERR);
      $('#storeAttributesTbl').hide();
    }
  };

  var evaluateDistance = function(latLng) {
    distMatrixService.getDistanceMatrix(
    {
      origins: [userLocation],
      destinations: [latLng],
      travelMode: google.maps.TravelMode.DRIVING
    }, storeFinder.distMatrixCB);
  };

  var directionsListener = function(result, status) {
    if (status == google.maps.DirectionsStatus.OK) {
      directionsDisplay.setMap(null);
      directionsDisplay.setMap(map);
      directionsDisplay.setDirections(result);
    }
  };

  var getDirections = function(latLng) {
    var request = {
      origin: userLocation,
      destination: latLng,
      travelMode: google.maps.TravelMode.DRIVING
    };
    directionsService.route(request, directionsListener);
  };

  var error = function(data, status){  // deliberate avoidance of callback hell with much nesting being avoided
    console.log('Error Data: ' + data.responseText + '\nStatus: ' + status );
    alert(data.responseText );
  };

  var success = function(data, status){  // deliberate avoidance of callback hell with much nesting being avoided
    function createMarker(store, index, array) {
      var latlng = new google.maps.LatLng(store.latitude, store.longitude);
      var key = buildKey(store);
      addMarker(key, latlng);
    }
    data.forEach(createMarker);
    stores = data;
    fetchStore( userLocation);
  };

  var fetchAllStores = function() {
    $.ajax({
      url: '/stores',
      type: 'GET',
      success: success,
      error: error});
  };

  var boundsChangedListener = function() {
    clearMarkers();
    if (this.zoom > ZOOM_LOWER_BOUND) {
      showMarkers();
    }
  };

  var userLocationCallback = function(position) {
    distMatrixService = new google.maps.DistanceMatrixService();
    directionsService = new google.maps.DirectionsService();

    userLocation = new google.maps.LatLng(position.coords.latitude, position.coords.longitude);
    mapOptions = {
      center:userLocation,
      zoom:STD_ZOOM,
      mapTypeId:google.maps.MapTypeId.HYBRID,
      mapTypeControl:true
    }
    map = new google.maps.Map(mapCanvas, mapOptions);
    map.addListener('bounds_changed', boundsChangedListener);

    var rendererOptions = { suppressMarkers: true };
    directionsDisplay = new google.maps.DirectionsRenderer(rendererOptions);
    directionsDisplay.setMap(map);
    userMarker = new google.maps.Marker(
      {position:userLocation,
       map:map,
       title:CURR_LOC_MARKER_TITLE,
       icon:GREEN_MARKER_ICON_URI});
    fetchAllStores();
    $(mapCanvas).removeAttr( 'hidden' );
  };

  var showGeoError = function(error) {
    defaultOntarioLocation = new google.maps.LatLng(DEFAULT_ON_LAT, DEFAULT_ON_LNG);
    userLocation = defaultOntarioLocation;
    fetchStore(defaultOntarioLocation);
  };

  // Adds a marker to the google map and add to our internal map.
  var addMarker = function(storeKey, location) {
    var marker = new google.maps.Marker({
      title: storeKey,
      position: location,
      map: map
    });
    marker.addListener('click', storeFinder.storeClickCB);
    markers.set(storeKey, marker);
  };

  // Sets the map on all markers in our collection.
  var setMapOnAll = function (map) {
    for (var marker of markers.values()) {
      marker.setMap(map);
    };
  };

  // Removes the markers from the map, but keeps them in our internal collection.
  var clearMarkers= function() {
    setMapOnAll(null);
  };

  // Shows any markers currently in our internal collection.
  var showMarkers = function() {
    setMapOnAll(map);
  };

  return {
    storeClickCB: function(e) {
      fetchStore(e.latLng);
      evaluateDistance(e.latLng);
      getDirections(e.latLng);
    },

    getTheSelectedStore: function() {
      if (theSelectedStore == null) return -1;
      return theSelectedStore.idField;
    },

    getTheSelectedLcboStoreId: function() {
      if (theSelectedStore == null) return -1;
      return theSelectedStore.lcbo_id;
    },

    distMatrixCB: function(response, status) {
      if (status == google.maps.DistanceMatrixStatus.OK) {
        var origins = response.originAddresses;
        if(origins.length > 0) {
          var results = response.rows[0].elements;
          if(results.length > 0 ) {
            var element = results[0];
            storeDistance = element.distance.text;
            $('#storeDistance').html(storeDistance);
          }
        }
      }
    },

    initMap: function() {
      if (navigator.geolocation) {
        navigator.geolocation.getCurrentPosition(userLocationCallback, showGeoError);
      } else  {
        $('#storeNearby').html(GEO_LOC_UNAVAILABLE);
        fetchStore(defaultOntarioLocation);
        $(mapCanvas).removeAttr( 'hidden' );
      }
    }
  }
}());



