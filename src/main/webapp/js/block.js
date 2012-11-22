
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
