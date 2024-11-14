import { LitElement, css, html } from "lit";
import { property } from "lit/decorators.js";

// =============================================================================
// WebComponent definition
// =============================================================================
export class NumberInput extends LitElement {
  static styles = css`
    input {
      padding: var(--size-2);
      border-radius: var(--radius-2);
      font-size: var(--font-size-1);
    }
    input:invalid {
      outline-offset: -2px;
      outline: var(--border-size-2) solid var(--red-6);
    }
    span {
      display: inline-block;
      font-size: var(--font-size-1);
      font-weight: var(--font-weight-3);
      color: var(--red-6);
      transform: scaleX(0);
      transition: transform 0.2s var(--ease-5);
      transform-origin: left;
    }
    input:invalid + span {
      transform: scaleX(1);
    }
  `;

  // Using the @property decorator requires the tsconfig.json file to have
  // { "experimentalDecorators": true }
  @property({ type: Number }) min: number = -Infinity;
  @property({ type: Number }) max: number = Infinity;
  @property({ type: Number }) value: number = 0;

  // This is a placeholder function that will be overwritten by the Shiny input
  // binding. When the input value changes, it invokes this function to notify
  // Shiny that it has changed.
  onChangeCallback = (x: boolean) => {};

  handleChange(e: InputEvent) {
    const rawValue = (e.target as HTMLInputElement).valueAsNumber;

    if (rawValue >= this.min && rawValue <= this.max) {
      this.value = rawValue;

      // Tell the Shiny input binding we've changed.
      this.onChangeCallback(true);
    }
  }

  render() {
    return html`
      <input
        value=${this.value}
        min=${this.min}
        max=${this.max}
        @input=${this.handleChange}
        type="number"
      />
      <span>Make sure your number is between ${this.min} and ${this.max}</span>
    `;
  }
}

// Register the custom element with the browser.
customElements.define("example-number-input", NumberInput);

// =============================================================================
// Register Shiny input binding
// =============================================================================
class ExampleNumberInputBinding extends Shiny["InputBinding"] {
  find(scope: HTMLElement): JQuery<HTMLElement> {
    return $(scope).find("example-number-input");
  }

  getValue(el: NumberInput) {
    return el.value;
  }

  subscribe(el: NumberInput, callback: (x: boolean) => void): void {
    el.onChangeCallback = callback;
  }

  unsubscribe(el: NumberInput): void {
    el.onChangeCallback = (x: boolean) => {};
  }
}

Shiny.inputBindings.register(
  new NumberInputBinding(),
  "NumberInputBindingBinding"
);
