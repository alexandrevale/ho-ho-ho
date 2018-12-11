$(document).ready(function() {
    $('#owl__quem-confia').owlCarousel({
        loop: true,
        margin: 15,
        nav: true,
        dots: false,
        responsiveClass:true,
        rewindNav:true,
        autoplay:true,
        autoplayTimeout:4000,    
        navText: ['<div class="left"></div>', '<div class="right"></div>'],
        responsive:{
            0:{
                items:1
            },
            991:{
                items:3
            },
            1000:{
                items:4
            }
        }
    })
    
    $('a[href*="#"]')
      .not('[href="#"]')
      .not('[href="#0"]')
      .click(function(event) {
      // On-page links
      if (
        location.pathname.replace(/^\//, '') == this.pathname.replace(/^\//, '') 
        && 
        location.hostname == this.hostname
      ) {
        // Figure out element to scroll to
        var target = $(this.hash);
        target = target.length ? target : $('[name=' + this.hash.slice(1) + ']');
        // Does a scroll target exist?
        if (target.length) {
          // Only prevent default if animation is actually gonna happen
          event.preventDefault();
          $('html, body').animate({
            scrollTop: target.offset().top
          }, 1000, function() {
            // Callback after animation
            // Must change focus!
            var $target = $(target);
            $target.focus();
            if ($target.is(":focus")) { // Checking if the target was focused
              return false;
            } else {
              $target.attr('tabindex','-1'); // Adding tabindex for elements not focusable
              $target.focus(); // Set focus again
            };
          });
        }
      }
    });
})