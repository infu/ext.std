import Array "mo:base/Array";
import Array_ "mo:array/Array";
import Base32 "mo:encoding/Base32";
import Binary "mo:encoding/Binary";
import Blob "mo:base/Blob";
import Char "mo:base/Char";
import CRC32 "mo:hash/CRC32";
import Hash "mo:base/Hash";
import Hex "mo:encoding/Hex";
import Iter "mo:base/Iter";
import Time "mo:base/Time";
import Nat8 "mo:base/Nat8";
import Nat32 "mo:base/Nat32";
import Principal "mo:principal/Principal";
import RawAccountId "mo:principal/AccountIdentifier";
import Result "mo:base/Result";
import Text "mo:base/Text";

// This modules follows Toniq Labs' EXT Standard.
// Lastest commit: 1f7ef3e.
module {
    public type AccountIdentifier = Text;
    public type SubAccount        = [Nat8];

    public func OptValid<A>(v:?A, f: (A) -> Bool) : Bool {
        switch(v) {case (?z) f(z); case(null) true }
    };

    public module AccountIdentifier = {
        public func equal(a : AccountIdentifier, b : AccountIdentifier) : Bool {
            let aRaw = switch (Hex.decode(a)) {
                case (#err(_)) { assert(false); []; };
                case (#ok(aR)) { aR; };
            };
            let bRaw = switch (Hex.decode(b)) {
                case (#err(_)) { assert(false); []; };
                case (#ok(bR)) { bR; };
            };
            RawAccountId.equal(aRaw, bRaw);
        };

        public func hash(a : AccountIdentifier) : Hash.Hash {
            let aRaw = switch (Hex.decode(a)) {
                case (#err(_)) { assert(false); []; };
                case (#ok(aR)) { aR; };
            };
            RawAccountId.hash(aRaw);
        };

        public func fromPrincipal(p : Principal, subAccount : ?SubAccount) : AccountIdentifier {
            RawAccountId.toText(RawAccountId.fromPrincipal(p, subAccount));
        };
    };

    // Balance refers to an amount of a particular token.
    public type Balance = Nat;

    public type CommonError = {
        #InvalidToken : TokenIdentifier;
        #Other        : Text;
    };

    public type Extension = Text;

    // Represents a 'payment' memo which can be attached to a transaction.
    public type Memo = Blob;

    // A unique id for a particular token and reflects the canister where the 
    // token resides as well as the index within the tokens container.
    public type TokenIdentifier = Text;

    public module TokenIdentifier = {
        private let prefix : [Nat8] = [10, 116, 105, 100]; // \x0A "tid"
     
        public func encode(canisterId : Principal, tokenIndex : TokenIndex) : Text {
            let rawTokenId = Array.flatten<Nat8>([
                prefix,
                Blob.toArray(Principal.toBlob(canisterId)),
                Binary.BigEndian.fromNat32(tokenIndex),
            ]);
            
            Principal.toText(Principal.fromBlob(Blob.fromArray(rawTokenId)));
        };

        public func decode(tokenId : TokenIdentifier) : Result.Result<(Principal, TokenIndex), Text> {
            let bs = Blob.toArray(Principal.toBlob(Principal.fromText(tokenId)));
            let (rawPrefix, rawToken) = Array_.split(bs, 4);
            if (rawPrefix != prefix) return #err("invalid prefix");
            let (rawCanister, rawIndex) = Array_.split(rawToken, rawToken.size() - 4 : Nat);
            #ok(
                Principal.fromBlob(Blob.fromArray(rawCanister)),
                Binary.BigEndian.toNat32(rawIndex),
            );
        };
    };

    // Represents an individual token's index within a given canister.
    public type TokenIndex = Nat32;

    public module TokenIndex = {
        public func equal(a : TokenIndex, b : TokenIndex) : Bool { a == b; };

        public func hash(a : TokenIndex) : Hash.Hash { a; };
    };

    public type User = {
        #address   : AccountIdentifier;
        #principal : Principal;
    };

    public module User = {
        public func equal(a : User, b : User) : Bool {
            let aAddress = toAccountIdentifier(a);
            let bAddress = toAccountIdentifier(b);
            AccountIdentifier.equal(aAddress, bAddress);
        };

        public func hash(u : User) : Hash.Hash {
            AccountIdentifier.hash(toAccountIdentifier(u));
        };

        public func toAccountIdentifier(u : User) : AccountIdentifier {
            switch (u) {
                case (#address(address)) { address; };
                case (#principal(principal)) {
                    AccountIdentifier.fromPrincipal(principal, null);
                };
            };
        };
    };

    public module Core = {
        public type BalanceRequest = { 
            user  : User; 
            token : TokenIdentifier;
        };

        public type BalanceResponse = Result.Result<
            Balance,
            CommonError
        >;

        public type BurnRequest = {
            user       : User;
            token      : TokenIdentifier;
            amount     : Balance;
            memo       : Memo;
            notify     : Bool;
            subaccount : ?SubAccount;
        };
        
        public type TransferRequest = {
            from       : User;
            to         : User;
            token      : TokenIdentifier;
            amount     : Balance;
            memo       : Memo;
            notify     : Bool;
            subaccount : ?SubAccount;
        };

        public type UseRequest = {
            user       : User;
            subaccount : ?SubAccount;
            token      : TokenIdentifier;
            memo       : Memo;
        };
        
        public type TransferLinkRequest = {
            from       : User;
            hash       : Blob;
            token      : TokenIdentifier;
            amount     : Balance;
            subaccount : ?SubAccount;
        };

        public type TransferResponse = Result.Result<Balance, {
            #Unauthorized : AccountIdentifier;
            #InsufficientBalance;
            #Rejected;
            #InvalidToken : TokenIdentifier;
            #CannotNotify : AccountIdentifier;
            #Other        : Text;
        }>;

        public type BurnResponse = TransferResponse;

        public type UseResponse = Result.Result<{
            #consumed;
            #cooldown: Nat32;
        }, {
            #Unauthorized : AccountIdentifier;
            #InsufficientBalance;
            #Rejected;
            #InvalidToken : TokenIdentifier;
            #OnCooldown;
            #ExtensionError: Text;
            #Other        : Text;
        }>;

        public type TransferLinkResponse = Result.Result<Nat32, {
            #Unauthorized : AccountIdentifier;
            #InsufficientBalance;
            #Rejected;
            #InvalidToken : TokenIdentifier;
            #Other        : Text;
        }>;

        public type ClaimLinkRequest = {
            to         : User;
            key        : Blob;
            token      : TokenIdentifier;
        };

         public type ClaimLinkResponse = Result.Result<(), {
            #Rejected; // We wont supply a possible attacker with various errors
         }>;
    };






    public type CustomId = Text;
    public module CustomId = {
        public func validate(t : CustomId) : Bool {
            t.size() <= 32 //TODO: Make real domain name verification.
        } 
    };

    public type Chunks = [Nat32];

    public type ContentType = Text;
    public module ContentType = {
        public func validate(t : ContentType) : Bool {
            t.size() <= 32 //TODO: Make real domain name verification.
        } 
    };
    
    public type ExternalRenderer = Principal;
    public type Content = {
        #internal: {
            contentType: ContentType;
            size: Nat32;
            };
        #external: {
            contentType: ContentType;
            idx: Nat32;
            };
        };

    public module Content = {
        public func validate(x : Content) : Bool {
            switch(x) {
                case (#internal({contentType; size})) {
                    ContentType.validate(contentType)
                };
                case (#external({contentType; idx})) {
                        ContentType.validate(contentType)
                }
            }
        }
    };

    public type EffectDesc = Text;
    public module EffectDesc = {
        public func validate(t : EffectDesc) : Bool {
            t.size() <= 256
        }
    };

    public type ItemUse = {
        #cooldown: {
            desc: EffectDesc;
            duration: Nat32;
            useId: CustomId;
        };

        #consumable : {
            desc: EffectDesc;
            useId: CustomId;
        };
    };
    
    public module ItemUse = {
        public func validate(t : ItemUse) : Bool {
            switch(t) {
                case (#cooldown({desc; duration; useId})) {
                    CustomId.validate(useId) and EffectDesc.validate(desc)
                };
                case (#consumable({desc; useId})) {
                    CustomId.validate(useId) and EffectDesc.validate(desc)
                }
            }
        }
    };

    public type ItemHold = {
            #external: {
            desc: EffectDesc;
            holdId: CustomId;
            }
    };
    public module ItemHold = {
        public func validate(t : ItemHold) : Bool {
            switch(t) {
                case (#external({desc; holdId})) {
                    CustomId.validate(holdId) and EffectDesc.validate(desc)
                };
            }
        }
    };

    public type ItemTransfer = {
        #unrestricted;
        #bindsForever;
        #bindsDuration: Nat32;
    };

    public type Attribute = (Text, Nat16);
    public module Attribute = {
        public func validate((a,n) : Attribute) : Bool {
            (a.size() <= 24)
        }
    };

    public type Attributes = [Attribute];
    public module Attributes = {
        public func validate(x : Attributes) : Bool {
            Iter.size(Iter.fromArray(x)) <= 10
            and
            Array.foldLeft(x, true, func (valid:Bool, val:Attribute) : Bool { 
                valid and Attribute.validate(val);
            })
        }
    };

    public type Sockets = [TokenIdentifier];
    public module Sockets = {
        public func validate(x : Sockets) : Bool {
            Iter.size(Iter.fromArray(x)) <= 10
        }
    };

    public type Tags = [Tag];
    public module Tags = {
        public func validate(x : Tags) : Bool {
            Iter.size(Iter.fromArray(x)) <= 10
            and
            Array.foldLeft(x, true, func (valid:Bool, val:Tag) : Bool {
                valid and Tag.validate(val);
            })
        }
    };

    public type Tag = Text;
    public module Tag = {
        public func validate(t : Tag) : Bool {
            t.size() <= 24
        }
    };

    public type ItemName = Text;
    public module ItemName = {
        public func validate(t : ItemName) : Bool {
            t.size() <= 96
        }
    };

    public type ItemLore = Text;
    public module ItemLore = {
        public func validate(t : ItemLore) : Bool {
            t.size() <= 256
        }
    };

    public type DomainName = Text;
    public module DomainName = {
        public func validate(t : DomainName) : Bool {
            t.size() <= 64 //TODO: Make real domain name verification.
        }
    };
        
    public type Metadata = {
        domain: ?DomainName;
        name: ?ItemName;
        lore: ?ItemLore;
        quality: Nat8;
        use: ?ItemUse;
        hold: ?ItemHold;
        transfer: ItemTransfer;
        ttl: ?Nat32; // time to live
        minter: Principal;
        extensionCanister: ?Principal;
        secret: Bool;
        content: ?Content;
        thumb: Content;    // may overwrite class
        entropy: Blob;
        created: Nat32; // in minutes
        attributes: Attributes;
        tags:Tags;
        custom: ?CustomData;
        // Idea: Have maturity rating
    };
        
    public type CustomData = Blob;
    public module CustomData = {
        public func validate(t : CustomData) : Bool {
            t.size() <= 1024 * 50 // 50 kb 
        }
    };

    public type MetadataInput = {
        domain: ?Text;
        name: ?Text;
        lore: ?Text;
        quality: Nat8;
        use: ?ItemUse;
        hold: ?ItemHold;
        secret: Bool;
        transfer: ItemTransfer;
        ttl: ?Nat32;
        content: ?Content;
        thumb: Content;
        extensionCanister: ?Principal;
        attributes: Attributes;
        tags: Tags;
        custom: ?CustomData;
    };

    public module MetadataInput = {
        public func validate(m : MetadataInput) : Bool {
            OptValid(m.domain, DomainName.validate)
            and OptValid(m.name, ItemName.validate)
            and OptValid(m.lore, ItemLore.validate)
            and OptValid(m.use, ItemUse.validate)
            and OptValid(m.hold, ItemHold.validate)
            and OptValid(m.content, Content.validate)
            and Content.validate(m.thumb)
            and Attributes.validate(m.attributes)
            and Tags.validate(m.tags)
            and OptValid(m.custom, CustomData.validate)
        }
    };

    public type Metavars = {
        var boundUntil: ?Nat32; // in minutes
        var cooldownUntil: ?Nat32; // in minutes
        var sockets: Sockets;
    };

    public type MetavarsFrozen = {
            boundUntil: ?Nat32; 
            cooldownUntil: ?Nat32; 
            sockets: Sockets;
    };

    public func MetavarsFreeze(a:Metavars) : MetavarsFrozen {
        {
            boundUntil= a.boundUntil; 
            cooldownUntil= a.cooldownUntil; 
            sockets= a.sockets; 
        }
    };

    public type MetadataResponse = Result.Result<
        {bearer: AccountIdentifier; data: Metadata; vars:MetavarsFrozen},
        CommonError
    >;

    public type SupplyResponse = Result.Result<
        Balance,
        CommonError
    >;
  
   public type PlugRequest = {
        user       : User;
        subaccount : ?SubAccount;
        socket : TokenIdentifier;
        plug   : TokenIdentifier;
    };

    public type PlugResponse = Result.Result<
        (), {
        #Rejected;
        #InsufficientBalance;
        #InvalidToken :TokenIdentifier;
        #Unauthorized :AccountIdentifier;
        #Other : Text;
        }
    >;

    public type SocketRequest = { 
        user       : User;
        subaccount : ?SubAccount;
        socket : TokenIdentifier;
        plug   : TokenIdentifier;
    };

    public type SocketResponse = Result.Result<
        (), {
        #Rejected;
        #InsufficientBalance;
        #Other : Text;
        #InvalidToken :TokenIdentifier;
        #Unauthorized :AccountIdentifier;
        }
    >;

    public type UnsocketRequest = {
        user       : User;
        subaccount : ?SubAccount;
        socket : TokenIdentifier;
        plug   : TokenIdentifier;
    };

    public type UnsocketResponse = Result.Result<
        (), {
        #Rejected;
        #InsufficientBalance;
        #InvalidToken :TokenIdentifier;
        #Unauthorized :AccountIdentifier;
        #Other :Text;
        }
    >;



    public type UnplugResponse = Result.Result<
        (), {
        #Rejected;
        #InsufficientBalance;
        #InvalidToken :TokenIdentifier;
        #Unauthorized :AccountIdentifier;
        #Other :Text;
        }
    >;


    public module NonFungible = {
        public type BearerResponse = Result.Result<
            AccountIdentifier, 
            CommonError
        >;

      

        public type UploadChunkRequest =  {
           tokenIndex: TokenIndex;
           position : {#content; #thumb};
           chunkIdx : Nat32;
           data : Blob;
        };

        public type FetchChunkRequest =  {
           tokenIndex: TokenIndex;
           position : {#content; #thumb};
           chunkIdx : Nat32;
           subaccount : ?SubAccount;
        };

        public type MintRequest = {
            to       : User;
            metadata : MetadataInput;
        };

        public type MintResponse = Result.Result<
           TokenIndex, {
            #Rejected;
            #InsufficientBalance;
            #Invalid: Text;
            #OutOfMemory;
          }
        >;

        public type MintBatchResponse = Result.Result<
           [TokenIndex],
           CommonError
        >;
    };

    public module Allowance = {
        public type Request = {
            owner   : User;
            spender : Principal;
            token   : TokenIdentifier;
        };

        public type Response = Result.Result<
            Balance,
            CommonError
        >;

        public type ApproveRequest = {
            subaccount : ?SubAccount;
            spender    : Principal;
            allowance  : Balance;
            token      : TokenIdentifier;
        };

        public type ApproveResponse = Result.Result<
            (),
            {
            #Other        : Text;
            #InvalidToken : TokenIdentifier;
            #Unauthorized : AccountIdentifier;
            #InsufficientBalance;
            }
        >;

    };
};
