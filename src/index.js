import { box, randomBytes } from "./nacl-fast.js";
import { decodeUTF8, encodeBase64 } from "./nacl-util.js";

const userPair = box.keyPair();
const serverPair = box.keyPair();

function encrypt(input, publicKey, secretKey) {
  const nonce = randomBytes(box.nonceLength);
  const messageUint8 = decodeUTF8(input);
  const encrypted = box(messageUint8, nonce, publicKey, secretKey);

  const fullMessage = new Uint8Array(
    nonce.length + encrypted.length + userPair.publicKey.length
  );
  fullMessage.set(nonce);
  fullMessage.set(encrypted, nonce.length);
  fullMessage.set(userPair.publicKey, nonce.length + encrypted.length);

  return encodeBase64(fullMessage);
}

const app = Elm.Main.init({
  node: document.querySelector("main"),
});
app.ports.encrypt.subscribe(function (input) {
  app.ports.encrypted.send(
    encrypt(JSON.stringify(input), serverPair.publicKey, userPair.secretKey)
  );
});
