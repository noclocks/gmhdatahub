var init = function() {

  console.log("(custom.js): Custom JS Initialized");
  // Add custom JS here
  $(document).on("shiny:connected", () => {
   console.log("(custom.js): Shiny App Connection Initialized.");
   // Add custom JS here
   });

   $(document).on("shiny:sessioninitialized", () => {
   console.log("(custom.js): Shiny App Session Initialized");
   // Add custom JS here
   });

   $(document).on("shiny:disconnected", () => {
     console.log("(custom.js): Shiny App Disconnected");
     // Add custom JS here
   });

   $(document).on("shiny:busy", () => {
     console.log("(custom.js): Shiny App Busy");
     // Add custom JS here
   });

   $(document).on("shiny:idle", () => {
     console.log("(custom.js): Shiny App Idle");
     // Add custom JS here
   });

};

export { init };
