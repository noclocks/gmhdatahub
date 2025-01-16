// src/bindings/index.js

import { addressInputBinding } from './address-input/address-input-binding';
import { emailInputBinding } from './email-input/email-input-binding';
import { phoneInputBinding } from './phone-input/phone-input-binding';
import { ratingInputBinding } from './rating-input/rating-input-binding';
import { urlInputBinding } from './url-input/url-input-binding';

Shiny.inputBindings.register(addressInputBinding);
Shiny.inputBindings.register(emailInputBinding);
Shiny.inputBindings.register(phoneInputBinding);
Shiny.inputBindings.register(ratingInputBinding);
Shiny.inputBindings.register(urlInputBinding);
