function onHover(plot) {
    $(document).ready( function() {
    $("#graphArea").bind("plothover", function(event, position, item) {


        var t = position.x
        $(".time").text(""+t)
        showFunction("Heart Rate",  ".heartRate",   ".hr",   t)
        showFunction("Power",       ".power",       ".pwr",  t)
        showFunction("Elevation",   ".elevation",   ".elev", t)
        showFunction("Temperature", ".temperature", ".temp", t)
    })

    function showFunction(fLabel, sectionCssSel, valueCssSel, t) {
        var y = getValue(fLabel,t)
        if (y != null) {
            $(sectionCssSel).show()
            $(valueCssSel).text(""+y)
        } else
            $(sectionCssSel).hide()
    }

    // return the function value of the function with label fLabel at time t
    function getValue(fLabel, t) {
        // var funcs = plot.getData()
        // alert(plot == undefined)

        return null
    }
})
}



// function flotSelection(selectF, unselectF, plot) {
//     $(document).ready(function() {
//         $("#graphArea").bind( "plotselected", function( event, ranges ) {
//             selectF(ranges.xaxis.from,ranges.xaxis.to)
//             // similar for yaxis - with multiple axes, the extra ones are in
//             // x2axis, x3axis, ...
//         })

//         $("#graphArea").bind( "plotunselected", function( event ) {
//             unselectF()
//         })




//     // function updateLegend() {
//     //     updateLegendTimeout = null

//     //     var pos = latestPosition

//     //     var axes = plot.getAxes()
//     //     if (pos.x < axes.xaxis.min || pos.x > axes.xaxis.max ||
//     //         pos.y < axes.yaxis.min || pos.y > axes.yaxis.max)
//     //         return

//     //     var i, j, dataset = plot.getData()
//     //     for (i = 0 i < dataset.length ++i) {
//     //         var series = dataset[i];

//     //         // find the nearest points, x-wise
//     //         for (j = 0; j < series.data.length; ++j)
//     //             if (series.data[j][0] > pos.x)
//     //                 break;

//     //         // now interpolate
//     //         var y, p1 = series.data[j - 1], p2 = series.data[j];
//     //         if (p1 == null)
//     //             y = p2[1];
//     //         else if (p2 == null)
//     //             y = p1[1];
//     //         else
//     //             y = p1[1] + (p2[1] - p1[1]) * (pos.x - p1[0]) / (p2[0] - p1[0]);

//     //         legends.eq(i).text(series.label.replace(/=.*/, "= " + y.toFixed(2)));
//     //     }





//     })
// }
