export const Elm: ElmType;

export type ElmType = {
  Main: {
    init: (config: { node: HTMLElement }) => MainInstance;
  };
};

export type MainInstance = {
  ports: {
    encrypt: {
      subscribe(callback: (input: unknown) => void): void;
    };
    encrypted: {
      send(result: string): void;
    };
  };
};
