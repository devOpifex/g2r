window.addEventListener('DOMContentLoaded', (event) => {
  var sliders = document.getElementsByClassName("g2-slider");

  for(let slider of sliders){
    let input = slider.getElementsByTagName("input");
    let label = document.getElementById(input[0].id + "-value");

    input[0].addEventListener("change", (ev) => {
      label.innerText = input[0].value;
    })
  }
});