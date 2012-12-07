
// Toggling of blocks
$(document).ready(function() {

    // Allow toggling all the blocks
    $(".block").each(function(i) {
        var button = $(this).find(".blocktoggle").first();
        var area   = $(this).find(".blockarea").first();

        var basePath = "/images/buttons/"

        button.click(function() {
            area.slideToggle("slow"); // toggle the area
            // switch the sprite
            if ($(this).hasClass('expand')) {
                $(this).removeClass('expand').addClass('collapse');
            } else if ($(this).hasClass('collapse')) {
                $(this).removeClass('collapse').addClass('expand');
            } else {
                $(this).addClass('collapse');
            }
        });
    });
});


// Stuff for autocompleting
function autocomplete(selector, names) {

  $(selector).autocomplete({
  source: names,
  focus: function( event, ui ) {
    $( selector ).val( ui.item.label );
    return false;
  }// ,
  // select: function( event, ui ) {
  //   $( selector ).val( ui.item.label );
  //   var url= ui.item.organizationId.split('"');
  //   //Here I redirect the page to the details url, passing the
  //   //Lift variable obfuscated name
  //   window.location=url[1];
  //   return false;
  // }
})
};
// .data( "autocomplete" )._renderItem = function( ul, item ) {
//   var itemDescription = self.buildItemDescription(item);
//   return $( "<li></li>" )
//     .data( "item.autocomplete", item )
//     .append( itemDescription )
//     .appendTo( ul );
//   };
// };
