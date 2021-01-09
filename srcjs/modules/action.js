import { getChart } from '../modules/shiny.js'
import { plot } from './plot.js';

const observeActions = () => {
  window.addEventListener('DOMContentLoaded', (event) => {
  
    let actions = document.getElementsByClassName("g2-actions");

    for(let i = 0; i < actions.length; i++){

      let id = actions[i].getAttribute("data-for");
      let x = JSON.parse(actions[i].innerHTML);
      let input = document.getElementById(x.input_id);
      let inputType = input.getAttribute("type");
      let c = getChart(id);
      let el = document.getElementById(id);

      if(inputType == "button"){
        input.addEventListener("click", () => {
          // main plot
          plot(c, x, el);
          c.render(true);
        })
      }

    }

  });
}

export { observeActions }