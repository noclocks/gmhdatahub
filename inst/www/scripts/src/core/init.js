// src/core/init.js
export function initializeApp() {

  $(document).on('shiny:connected', function (event) {
    console.log('Shiny connected');
  });

  $(document).on("shiny:sessioninitialized", function (event) {
    console.log("(init.js): Shiny App Session Initialized");
  });

  $(document).on("shiny:disconnected", function (event) {
    console.log("(init.js): Shiny App Disconnected");
  });

  $(document).on("shiny:busy", function (event) {
    console.log("(init.js): Shiny App Busy");
  });

  $(document).on("shiny:idle", function (event) {
    console.log("(init.js): Shiny App Idle");
  });

};
