function linear(p,q,alpha) { return (1 - alpha) * p + alpha * q }

function onHover(plot) {
    $(document).ready( function() {
    $("#graphArea").bind("plothover", function(event, position, item) {

        var axes = plot.getAxes()
        var t = Math.min(Math.max(axes.xaxis.min, position.x), axes.xaxis.max)

        $(".time").text(showDate(new Date(Math.floor(t))))

        showFunction("Speed",       "#graphIndicator .timing",      t)
        showFunction("Heart Rate",  "#graphIndicator .heartRate",   t)
        showFunction("Power",       "#graphIndicator .power",       t)
        showFunction("Altitude",    "#graphIndicator .elevation",   t)
        showFunction("Temperature", "#graphIndicator .temperature", t)
    })

    function showFunction(fLabel, sectionCssSel, t) {
        var y = getValue(fLabel,t)
        if (y != null) {
            $(sectionCssSel).show()
            $(sectionCssSel + " .value").text(""+y)
        } else
            $(sectionCssSel).hide()
    }

    // return the function value of the function with label fLabel at time t
    function getValue(fLabel, t) {
        var series = plot.getData();
        for (var i = 0; i < series.length; ++i)
            if (series[i].label == fLabel) {
                return getValueFromSeries(series[i],t)
            }
        return null
    }

    function getValueFromSeries(series, t) {
        function inBetween(p,q) {
            return p == q ? p : linear(p[1],q[1], (t - p[0]) / (q[0] -p[0]))
        }

        var i = 0

        while( i < series.data.length && series.data[i][0] <= t){
            i++;
        }

        // alert("i: "+ i + "[i-1]: "+ series.data[i-1] + " [i]: " + series.data[i])

        var p = series.data[i-1]
        var q = series.data[Math.min(i,series.data.length-1)]

        // alert("p: "+p+" q: "+q)

        return inBetween(p,q).toFixed(1)
    }

    function showDate(d) {
        function pad(x) {
            return ("0" + x).substr(-2,2)
        }
        // TODO: checkout the weird issue with the hours
        return pad(d.getHours() - 1) + ":" + pad(d.getMinutes()) + ":" + pad(d.getSeconds())
    }
})
}
