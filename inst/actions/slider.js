document.onload = function(){

  var sliders = document.getElementsByClassName("g2-slider");

  sliders.forEach((slider) => {
    let input = slider.getElementsByTagName("input");
    let label = slider.getElementsByTagName("label");

    input[0].addEventListener("change", (ev) => {
      label[0].innerText = input[0].value;
    })
  })

}
