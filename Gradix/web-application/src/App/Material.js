//'use strict';
const mdcTextField = require('@material/textfield');
const mdcMenu = require('@material/menu');
const mdcRipple = require ('@material/ripple');
const MDCTextField = mdcTextField.MDCTextField;
const MDCMenu = mdcMenu.MDCMenu;
const MDCRipple = mdcRipple.MDCRipple;
exports.upgradeElements = function () 
{
        for (const el of document.querySelectorAll('.mdc-text-field')) {
          const textField = new MDCTextField(el);}
        for(const el of document.querySelectorAll('.mdc-menu')){
          const menu = new MDCMenu(el);
          menu.open = true;
        }
        for(const el of document.querySelectorAll('.mdc-button')){
          const button = new MDCRipple(el);
        }
        console.log("upgrade Finished!");
  }

