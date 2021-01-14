// const DARK = {
//   background: 'transparent',
//   defaultColor: '#ff9800',
//   colors10: [
//     "#ffadad",
//     "#ffd6a5",
//     "#fdffb6",
//     "#caffbf",
//     "#9bf6ff",
//     "#a0c4ff",
//     "#bdb2ff",
//     "#ffc6ff"
//   ],
//   geometries: {
//     point: {
//       "hollow-circle": {
//         default: {
//           style: {
//             stroke: '#ff9800'
//           }
//         }
//       }
//     },
//     line: {
//       line: {
//         default: {
//           style: {
//             stroke: '#ff9800'
//           }
//         }
//       }
//     }
//   }
// };

// window.onload = (event) => {
//   let theme = "light";
//   if (
//     window.matchMedia &&
//     window.matchMedia("(prefers-color-scheme: dark)").matches
//   ) {
//     theme = "darkly";
//   }

//   if(theme === "light")
//     return ;

//   if(window.location.pathname.includes("motif"))
//     return ;

//   let elements = document.getElementsByClassName("g2r");

//   for (let el of elements){
//     let widget = HTMLWidgets.find("#" + el.id);
//     let c = widget.getChart();
//     c.theme(DARK);
//     c.render();
//   }
// };