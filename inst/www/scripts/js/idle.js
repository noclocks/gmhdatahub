/**
 * Inactive Idle Timer
 */

 function idleTimer(milliseconds = 120000) {

   // time is in milliseconds (1000 is 1 second)
   var t = setTimeout(logout, milliseconds);

   // catch mouse movements
   window.onmousemove = resetTimer;
   window.onmousedown = resetTimer;
   window.onclick = resetTimer;
   window.onscroll = resetTimer;
   window.onkeypress = resetTimer;

   function logout() {
     window.close();
   }

   function resetTimer() {
     clearTimeout(t);
     t = setTimeout(logout, milliseconds);
   }

 }

 idleTimer();
