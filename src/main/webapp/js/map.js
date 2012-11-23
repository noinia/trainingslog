

function drawMap(area, locations) {
    $(document).ready(function() {
        $(area).gmap().bind('init', function(ev, map) {
            $(area).gmap('addMarker',
                         {'position': '57.7973333,12.0502107',
                          'bounds': true});
        });
    });
}




        // var myOptions = {
        //     zoom: 12,
        //     mapTypeId: google.maps.MapTypeId.ROADMAP
        // };

        // var mapArea = $(area);
        // var map = new google.maps.Map(mapArea.get(), myOptions);
        // initialLocation = new google.maps.LatLng(locations.loc[0].lat, locations.loc[0].lng);
        // map.setCenter(initialLocation);
        // for(i=0; i<locations.loc.length; i++) {
        //     var point = new google.maps.LatLng(locations.loc[i].lat,locations.loc[i].lng);

        //     var marker = new google.maps.Marker({position: point,
        //                                          title: locations.loc[i].title });
        //     marker.setMap(map);
        // }
        // alert("woei");
