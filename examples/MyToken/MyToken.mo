import Ext "mo:std/EXT/Ext";
import Interface "mo:std/EXT/Interface";

shared({caller = owner}) actor class Token() : async Interface.FungibleToken = {
    // TODO: implement endpoints.

    // @ext:core
    public query func balance(request : Ext.Core.BalanceRequest) : async Ext.Core.BalanceResponse {
        #err(#Other("not implemented"));
    };

    public query func extensions() : async [Ext.Core.Extension] {
        ["@ext:common"];
    };

    public shared({caller}) func transfer(request : Ext.Core.TransferRequest) : async Ext.Core.TransferResponse {
        #err(#Other("not implemented"));
    };

    // @ext:common
    public query func metadata(token : Ext.Core.TokenIdentifier) : async Ext.Common.MetadataResponse {
        #err(#Other("not implemented"));
    };

    public query func supply(token : Ext.Core.TokenIdentifier) : async Ext.Common.SupplyResponse {
        #err(#Other("not implemented"));
    };
};
