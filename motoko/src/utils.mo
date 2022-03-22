import Text "mo:base/Text";
import Principal "mo:base/Principal";
import List "mo:base/List";
import Array "mo:base/Array";
import Blob "mo:base/Blob";
import Prim "mo:⛔";
import Base32 "mo:base32/Base32";
import CRC32 "mo:commons/CRC32";

/// reference https://github.com/Deland-Labs/dfinity-fungible-token-standard/blob/main/motoko/dft/utils/PrincipalExt.mo
module {
    let CRC_LENGTH_IN_BYTES: Nat = 4;
    let CANISTER_ID_HASH_LEN_IN_BYTES: Nat = 10;
    let HASH_LEN_IN_BYTES: Nat = 28;
    let MAX_LENGTH_IN_BYTES: Nat = 29; //HASH_LEN_IN_BYTES + 1; // 29
    let TYPE_SELF_AUTH: Nat8 = 0x02;

    public func textToPrincipal(text : Text) : ?Principal {
      var _text = Text.map(text , Prim.charToLower);
      _text := Text.replace(_text , #text "-" , "");
      let decodeResult = Base32.decode(#RFC4648({ padding=false; }),_text);
      let bytes:[Nat8] = switch (decodeResult)
      {
        case null [];
        case (?b) b;
      };
      
      let bytesSize = bytes.size();

      if ( bytes.size() < CRC_LENGTH_IN_BYTES ) { return null; }
      else if ( bytes.size() > MAX_LENGTH_IN_BYTES + CRC_LENGTH_IN_BYTES) { return null; }
      else if ( text == "aaaaa-aa") { return ?Principal.fromText(text); }
      else {
        let body = Array.init<Nat8>(bytesSize - 4, 0) ;
      
        for (k in bytes.keys()) {     
           if ( k > 3 ) { 
             body[ k - 4 ] := bytes [ k ];
           }
        };

        let crcResult : [Nat8] = CRC32.crc32(Array.freeze(body));

        for (c in crcResult.keys()){
          if ( bytes[c] != crcResult[c]) {
            return null;
          }
        };
        return ?Principal.fromText(text);
      };
    };
    
    public func isAccountIdentifier(t: Text): Bool {
        if (Text.size(t) != 64) {
            return false;
        };
        let it = Text.toIter(t);
        var arr: [Nat8] = [];
        for (c in it) {
            switch(c) {
                case ('0') {
                    arr := Array.append<Nat8>(arr, [0]);
                };
                case ('1') {
                    arr := Array.append<Nat8>(arr, [1]);
                };
                case ('2') {
                    arr := Array.append<Nat8>(arr, [2]);
                };
                case ('3') {
                    arr := Array.append<Nat8>(arr, [3]);
                };
                case ('4') {
                    arr := Array.append<Nat8>(arr, [4]);
                };
                case ('5') {
                    arr := Array.append<Nat8>(arr, [5]);
                };
                case ('6') {
                    arr := Array.append<Nat8>(arr, [6]);
                };
                case ('7') {
                    arr := Array.append<Nat8>(arr, [7]);
                };
                case ('8') {
                    arr := Array.append<Nat8>(arr, [8]);
                };
                case ('9') {
                    arr := Array.append<Nat8>(arr, [9]);
                };
                case ('a') {
                    arr := Array.append<Nat8>(arr, [10]);
                };
                case ('b') {
                    arr := Array.append<Nat8>(arr, [11]);
                };
                case ('c') {
                    arr := Array.append<Nat8>(arr, [12]);
                };
                case ('d') {
                    arr := Array.append<Nat8>(arr, [13]);
                };
                case ('e') {
                    arr := Array.append<Nat8>(arr, [14]);
                };
                case ('f') {
                    arr := Array.append<Nat8>(arr, [15]);
                };
                case (_) {
                    return false;
                };
            }
        };
        return true;
    }
    
}