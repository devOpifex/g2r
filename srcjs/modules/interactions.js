import { checkType } from "./utils.js";
import { registerInteraction } from "@antv/g2";

const interactions = (v, options) => {
  if (options === undefined) return;

  if (checkType(options)) v.interaction(options[0], options[1]);
  else v.interaction(options);
};

const registerInteractions = (options) => {
  if (options === undefined) return;

  options.forEach((reg) => {
    registerInteraction(reg.name, reg.opts);
  });
};

export { interactions, registerInteractions };
