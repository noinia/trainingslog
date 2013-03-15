

function drawMap(area,locations) {
    $(document).ready(function () {
        function zoomToFit(map, locs) {
            var bounds = new google.maps.LatLngBounds()
            for (var i=0 ; i < locs.length ; i++ )
                bounds.extend(locs[i])
            map.fitBounds(bounds)
        }

        function placeMarker(m, t) {
            var i = 0
            while( i < locations.length && locations[i].time <= t){
                i++;
            }
            m.setPosition(locations[i-1].position)
        }


        var positions = locations.map(function (x) {return x.position})

        var map = new google.maps.Map(area, {
              mapTypeId: google.maps.MapTypeId.ROADMAP
        })

        var path = new google.maps.Polyline({
              path         : positions
            , strokeColor  : "#FF0000"
            , strokeOpacity: 1.0
            , strokeWeight : 2
            , map          : map
        })

        zoomToFit(map,positions)

        var startMarker = new google.maps.Marker({
              map       : map
            , position  : positions[0]
            , title     : "Start"
        })
        var endMarker = new google.maps.Marker({
              map       : map
            , position  : positions[positions.length-1]
            , title     : "End"
        })

        var currentPostion = new google.maps.Marker({
              map       : map
            , position  : positions[0]
        })

        $("#graphArea").bind("plothover", function(event, position, item) {
            // var axes = plot.getAxes()
            // var t = Math.min(Math.max(axes.xaxis.min, position.x), axes.xaxis.max)
            placeMarker(currentPostion,Math.round(position.x))
        })

    })
}
