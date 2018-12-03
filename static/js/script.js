function mostra(){
    var d1 = document.getElementById("d1");
    d1.addEventListener("click",function(e){
       alert(e.target.innerHTML); 
    });
}

window.load = mostra;