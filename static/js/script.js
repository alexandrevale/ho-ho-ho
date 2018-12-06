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
})