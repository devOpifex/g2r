import { registerTheme } from '@antv/g2';

// no need to register the theme multiple times
let registered = [];

const observeThemes = () => {
  window.addEventListener('DOMContentLoaded', (event) => {
  
    let themes = document.getElementsByClassName("g2-themes");

    for(let i = 0; i < themes.length; i++){

      let themeObj = JSON.parse(themes[i].innerHTML);

      if(registered.includes(themeObj.name))
        continue;

      registerTheme(themeObj.name, themeObj.opts);

      registered.push(themeObj.name);

    }

  });
}

export { observeThemes }
