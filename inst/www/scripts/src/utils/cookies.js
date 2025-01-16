// src/utils/cookies.js

export function setSessionCookie(sessionId) {
  try {
    const date = new Date();
    date.setTime(date.getTime() + (30 * 24 * 60 * 60 * 1000)); // 30 days
    const cookieValue = "shiny_session=" + sessionId + ";expires=" + date.toUTCString() + ";path=/;SameSite=Strict;Secure";
    document.cookie = cookieValue;
    console.log("Session Cookie Set Successfully: " + cookieValue);
  } catch (e) {
    console.error("Error setting session cookie: ", e);
  }
}

export function getSessionCookie() {
  try {
    const name = "shiny_session=";
    const decodedCookie = decodeURIComponent(document.cookie);
    const cookieArray = decodedCookie.split(';');
    for (let i = 0; i < cookieArray.length; i++) {
      let cookie = cookieArray[i];
      while (cookie.charAt(0) === ' ') {
        cookie = cookie.substring(1);
      }
      if (cookie.indexOf(name) === 0) {
        return cookie.substring(name.length, cookie.length);
      }
    }
    return null;
  } catch (e) {
    console.error("Error getting session cookie: ", e);
    return null;
  }
}

Shiny.addCustomMessageHandler("setSessionCookie", function (message) {
  if (message.sessionId) {
    setSessionCookie(message.sessionId);
  }
});

Shiny.addCustomMessageHandler("getSessionCookie", function (message) {
  const sessionId = getSessionCookie();
  Shiny.setInputValue("sessionCookie", sessionId);
});

