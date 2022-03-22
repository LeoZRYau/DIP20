/**
 * Module     : token.mo
 * Copyright  : 2021 DFinance Team
 * License    : Apache 2.0 with LLVM Exception
 * Maintainer : DFinance Team <hello@dfinance.ai>
 * Stability  : Experimental
 */

import HashMap "mo:base/HashMap";
import Principal "mo:base/Principal";
import Types "./types";
import Time "mo:base/Time";
import Iter "mo:base/Iter";
import Array "mo:base/Array";
import Option "mo:base/Option";
import Order "mo:base/Order";
import Nat "mo:base/Nat";
import Nat64 "mo:base/Nat64";
import Result "mo:base/Result";
import Text "mo:base/Text";
import ExperimentalCycles "mo:base/ExperimentalCycles";
import Cap "./cap/Cap";
import Root "./cap/Root";
import Utils "./utils";
import PrincipalUtils "mo:commons/PrincipalUtils";
import Debug "mo:base/Debug";

shared(msg) actor class Token(
    _logo: Text,
    _name: Text,
    _symbol: Text,
    _decimals: Nat8,
    _totalSupply: Nat,
    _owner: Principal,
    _fee: Nat
    ) = this {
    type AccountIdentifier = Types.AccountIdentifier;
    type Operation = Types.Operation;
    type TransactionStatus = Types.TransactionStatus;
    type TxRecord = Types.TxRecord;
    type Metadata = {
        logo : Text;
        name : Text;
        symbol : Text;
        decimals : Nat8;
        totalSupply : Nat;
        owner : Principal;
        fee : Nat;
    };
    // returns tx index or error msg
    public type TxReceipt = {
        #Ok: Nat;
        #Err: {
            #InsufficientAllowance;
            #InsufficientBalance;
            #ErrorOperationStyle;
            #Unauthorized;
            #LedgerTrap;
            #ErrorTo;
            #Other: Text;
            #BlockUsed;
            #AmountTooSmall;
            #InvalidAddress: Text;
        };
    };
    private stable var owner_ : Principal = _owner;
    private stable var ownerAccount: AccountIdentifier = PrincipalUtils.toAddress(owner_); 
    private stable var logo_ : Text = _logo;
    private stable var name_ : Text = _name;
    private stable var decimals_ : Nat8 = _decimals;
    private stable var symbol_ : Text = _symbol;
    private stable var totalSupply_ : Nat = _totalSupply;
    private stable var blackhole : AccountIdentifier = PrincipalUtils.toAddress(Principal.fromText("aaaaa-aa"));
    private stable var feeTo : Principal = owner_;
    private stable var feeToAccount: AccountIdentifier = PrincipalUtils.toAddress(feeTo); 
    private stable var fee : Nat = _fee;
    private stable var balanceEntries : [(AccountIdentifier, Nat)] = [];
    private stable var allowanceEntries : [(AccountIdentifier, [(AccountIdentifier, Nat)])] = [];

    private var balances = HashMap.HashMap<AccountIdentifier, Nat>(1, Text.equal, Text.hash);
    private var allowances = HashMap.HashMap<AccountIdentifier, HashMap.HashMap<AccountIdentifier, Nat>>(1, Text.equal, Text.hash);

    balances.put(ownerAccount, totalSupply_);
    private stable let genesis : TxRecord = {
        caller = ?owner_;
        op = #mint;
        index = 0;
        from = blackhole;
        to = ownerAccount;
        amount = totalSupply_;
        fee = 0;
        timestamp = Time.now();
        status = #succeeded;
    };
    
    private stable var txcounter: Nat = 0;
    private var cap: ?Cap.Cap = null;
    private func addRecord(
        caller: Principal,
        op: Text, 
        details: [(Text, Root.DetailValue)]
        ): async () {
        let c = switch(cap) {
            case(?c) { c };
            case(_) { Cap.Cap(Principal.fromActor(this), 2_000_000_000_000) };
        };
        cap := ?c;
        let record: Root.IndefiniteEvent = {
            operation = op;
            details = details;
            caller = caller;
        };
        // don't wait for result, faster
        ignore c.insert(record);
    };

    private func _chargeFee(from: AccountIdentifier, fee: Nat) {
        if(fee > 0) {
            _transfer(from, feeToAccount, fee);
        };
    };

    private func _transfer(from: AccountIdentifier, to: AccountIdentifier, value: Nat) {
        let from_balance = _balanceOf(from);
        let from_balance_new : Nat = from_balance - value;
        if (from_balance_new != 0) { balances.put(from, from_balance_new); }
        else { balances.delete(from); };

        let to_balance = _balanceOf(to);
        let to_balance_new : Nat = to_balance + value;
        if (to_balance_new != 0) { balances.put(to, to_balance_new); };
    };

    private func _balanceOf(who: AccountIdentifier) : Nat {
        switch (balances.get(who)) {
            case (?balance) { return balance; };
            case (_) { return 0; };
        }
    };

    private func _allowance(owner: AccountIdentifier, spender: AccountIdentifier) : Nat {
        switch(allowances.get(owner)) {
            case (?allowance_owner) {
                switch(allowance_owner.get(spender)) {
                    case (?allowance) { return allowance; };
                    case (_) { return 0; };
                }
            };
            case (_) { return 0; };
        }
    };

    private func u64(i: Nat): Nat64 {
        Nat64.fromNat(i)
    };

    /*
    *   Core interfaces:
    *       update calls:
    *           transfer/transferFrom/approve
    *       query calls:
    *           logo/name/symbol/decimal/totalSupply/balanceOf/allowance/getMetadata
    *           historySize/getTransaction/getTransactions
    */

    /// Transfers value amount of tokens to Principal to.
    public shared(msg) func transfer(to: Text, value: Nat) : async TxReceipt {

        let _to: AccountIdentifier = switch(Utils.textToPrincipal(to)) { 
            case (?p) {
                PrincipalUtils.toAddress(p);
            }; 
            case (_) {
                if (Utils.isAccountIdentifier(to)) {
                    to;
                } else {
                    return #Err(#InvalidAddress(to));
                };
            }
        };
        Debug.print("==>transfer._to=" # _to);

        let _caller: Principal = msg.caller;
        let _from: AccountIdentifier  = PrincipalUtils.toAddress(_caller);
        
        if (_balanceOf(_from) < value + fee) { 
            return #Err(#InsufficientBalance); 
        };
        _chargeFee(_from, fee);
        _transfer(_from, _to, value);
        // TODO 0 check is the record stores in cap..
        ignore addRecord(
            _caller, "transfer",
            [
                ("to", #Text(_to)),
                ("value", #U64(u64(value))),
                ("fee", #U64(u64(fee)))
            ]
        );
        txcounter += 1;
        return #Ok(txcounter - 1);
    };

    /// Transfers value amount of tokens from Principal from to Principal to.
    public shared(msg) func transferFrom(from: Text, to: Text, value: Nat) : async TxReceipt {

        let _from: AccountIdentifier = switch(Utils.textToPrincipal(from)) { 
            case (?p) {
                PrincipalUtils.toAddress(p);
            }; 
            case (_) {
                if (Utils.isAccountIdentifier(from)) {
                    from;
                } else {
                    return #Err(#InvalidAddress(from));
                };
            }
        };

        let _to: AccountIdentifier = switch(Utils.textToPrincipal(to)) { 
            case (?p) {
                PrincipalUtils.toAddress(p);
            }; 
            case (_) {
                if (Utils.isAccountIdentifier(to)) {
                    to;
                } else {
                    return #Err(#InvalidAddress(to));
                };
            }
        };
        Debug.print("==>transferFrom. _from=" # _from # ", _to=" # _to);
        let _caller = msg.caller;
        let _callerAccount = PrincipalUtils.toAddress(_caller);

        if (_balanceOf(_from) < value + fee) { 
            return #Err(#InsufficientBalance); 
        };
        let allowed : Nat = _allowance(_from, _callerAccount);
        if (allowed < value + fee) { 
            return #Err(#InsufficientAllowance); 
        };

        _chargeFee(_from, fee);
        _transfer(_from, _to, value);

        let allowed_new : Nat = allowed - value - fee;
        if (allowed_new != 0) {
            let allowance_from = Types.unwrap(allowances.get(_from));
            allowance_from.put(_callerAccount, allowed_new);
            allowances.put(from, allowance_from);
        } else {
            if (allowed != 0) {
                let allowance_from = Types.unwrap(allowances.get(_from));
                allowance_from.delete(_callerAccount);
                if (allowance_from.size() == 0) { allowances.delete(_from); }
                else { allowances.put(_from, allowance_from); };
            };
        };
        ignore addRecord(
            msg.caller, "transferFrom",
            [
                ("from", #Text(_from)),
                ("to", #Text(_to)),
                ("value", #U64(u64(value))),
                ("fee", #U64(u64(fee)))
            ]
        );
        txcounter += 1;
        return #Ok(txcounter - 1);
    };

    /// Allows spender to withdraw from your account multiple times, up to the value amount.
    /// If this function is called again it overwrites the current allowance with value.
    public shared(msg) func approve(spender: Text, value: Nat) : async TxReceipt {
        let _spender: AccountIdentifier = switch(Utils.textToPrincipal(spender)) { 
            case (?p) {
                PrincipalUtils.toAddress(p);
            }; 
            case (_) {
                if (Utils.isAccountIdentifier(spender)) {
                    spender;
                } else {
                    return #Err(#InvalidAddress(spender));
                };
            }
        };
        Debug.print("==>approve. _spender=" # _spender);
        let _caller = msg.caller;
        let _callerAccount = PrincipalUtils.toAddress(_caller);

        if(_balanceOf(_callerAccount) < fee) { 
            return #Err(#InsufficientBalance); 
        };
        _chargeFee(_callerAccount, fee);
        let v = value + fee;
        if (value == 0 and Option.isSome(allowances.get(_callerAccount))) {
            let allowance_caller = Types.unwrap(allowances.get(_callerAccount));
            allowance_caller.delete(_spender);
            if (allowance_caller.size() == 0) { allowances.delete(_callerAccount); }
            else { allowances.put(_callerAccount, allowance_caller); };
        } else if (value != 0 and Option.isNull(allowances.get(_callerAccount))) {
            var temp = HashMap.HashMap<AccountIdentifier, Nat>(1, Text.equal, Text.hash);
            temp.put(_spender, v);
            allowances.put(_callerAccount, temp);
        } else if (value != 0 and Option.isSome(allowances.get(_callerAccount))) {
            let allowance_caller = Types.unwrap(allowances.get(_callerAccount));
            allowance_caller.put(_spender, v);
            allowances.put(_callerAccount, allowance_caller);
        };
        ignore addRecord(
            msg.caller, "approve",
            [
                ("to", #Text(_spender)),
                ("value", #U64(u64(value))),
                ("fee", #U64(u64(fee)))
            ]
        );
        txcounter += 1;
        return #Ok(txcounter - 1);
    };

    public shared(msg) func mint(to: Text, value: Nat): async TxReceipt {
        if(msg.caller != owner_) {
            return #Err(#Unauthorized);
        };
        let _to: AccountIdentifier = switch(Utils.textToPrincipal(to)) { 
            case (?p) {
                PrincipalUtils.toAddress(p);
            }; 
            case (_) {
                if (Utils.isAccountIdentifier(to)) {
                    to;
                } else {
                    return #Err(#InvalidAddress(to));
                };
            }
        };
        Debug.print("==>transfer._to=" # _to);

        
        let to_balance = _balanceOf(_to);
        totalSupply_ += value;
        balances.put(_to, to_balance + value);
        ignore addRecord(
            msg.caller, "mint",
            [
                ("to", #Text(_to)),
                ("value", #U64(u64(value))),
                ("fee", #U64(u64(0)))
            ]
        );
        txcounter += 1;
        return #Ok(txcounter - 1);
    };

    public shared(msg) func burn(amount: Nat): async TxReceipt {
        let _caller = msg.caller;
        let _callerAccount = PrincipalUtils.toAddress(_caller);

        let from_balance = _balanceOf(_callerAccount);
        if(from_balance < amount) {
            return #Err(#InsufficientBalance);
        };
        totalSupply_ -= amount;
        balances.put(_callerAccount, from_balance - amount);
        ignore addRecord(
            _caller, "burn",
            [
                ("from", #Principal(msg.caller)),
                ("value", #U64(u64(amount))),
                ("fee", #U64(u64(0)))
            ]
        );
        txcounter += 1;
        return #Ok(txcounter - 1);
    };

    public query func logo() : async Text {
        return logo_;
    };

    public query func name() : async Text {
        return name_;
    };

    public query func symbol() : async Text {
        return symbol_;
    };

    public query func decimals() : async Nat8 {
        return decimals_;
    };

    public query func totalSupply() : async Nat {
        return totalSupply_;
    };

    public query func getTokenFee() : async Nat {
        return fee;
    };

    public query func balanceOf(who: Text) : async Nat {
        let _who: AccountIdentifier = switch(Utils.textToPrincipal(who)) { 
            case (?p) {
                PrincipalUtils.toAddress(p);
            }; 
            case (_) {
                if (Utils.isAccountIdentifier(who)) {
                    who;
                } else {
                    return 0;
                };
            }
        };
        return _balanceOf(_who);
    };

    public query func allowance(owner: Text, spender: Text) : async Nat {
        let _owner: AccountIdentifier = switch(Utils.textToPrincipal(owner)) { 
            case (?p) {
                PrincipalUtils.toAddress(p);
            }; 
            case (_) {
                if (Utils.isAccountIdentifier(owner)) {
                    owner;
                } else {
                    return 0;
                };
            }
        };
        let _spender: AccountIdentifier = switch(Utils.textToPrincipal(spender)) { 
            case (?p) {
                PrincipalUtils.toAddress(p);
            }; 
            case (_) {
                if (Utils.isAccountIdentifier(spender)) {
                    spender;
                } else {
                    return 0;
                };
            }
        };
        return _allowance(_owner, _spender);
    };

    public query func getMetadata() : async Metadata {
        return {
            logo = logo_;
            name = name_;
            symbol = symbol_;
            decimals = decimals_;
            totalSupply = totalSupply_;
            owner = owner_;
            fee = fee;
        };
    };

    /// Get transaction history size
    public query func historySize() : async Nat {
        return txcounter;
    };

    /*
    *   Optional interfaces:
    *       setName/setLogo/setFee/setFeeTo/setOwner
    *       getUserTransactionsAmount/getUserTransactions
    *       getTokenInfo/getHolders/getUserApprovals
    */
    public shared(msg) func setName(name: Text) {
        assert(msg.caller == owner_);
        name_ := name;
    };

    public shared(msg) func setLogo(logo: Text) {
        assert(msg.caller == owner_);
        logo_ := logo;
    };

    public shared(msg) func setFeeTo(to: Principal) {
        assert(msg.caller == owner_);
        feeTo := to;
        feeToAccount := PrincipalUtils.toAddress(feeTo);
    };

    public shared(msg) func setFee(_fee: Nat) {
        assert(msg.caller == owner_);
        fee := _fee;
    };

    public shared(msg) func setOwner(_owner: Principal) {
        assert(msg.caller == owner_);
        owner_ := _owner;
    };

    public type TokenInfo = {
        metadata: Metadata;
        feeTo: Principal;
        // status info
        historySize: Nat;
        deployTime: Time.Time;
        holderNumber: Nat;
        cycles: Nat;
    };
    public query func getTokenInfo(): async TokenInfo {
        {
            metadata = {
                logo = logo_;
                name = name_;
                symbol = symbol_;
                decimals = decimals_;
                totalSupply = totalSupply_;
                owner = owner_;
                fee = fee;
            };
            feeTo = feeTo;
            historySize = txcounter;
            deployTime = genesis.timestamp;
            holderNumber = balances.size();
            cycles = ExperimentalCycles.balance();
        }
    };

    public query func getHolders(start: Nat, limit: Nat) : async [(AccountIdentifier, Nat)] {
        let temp =  Iter.toArray(balances.entries());
        func order (a: (AccountIdentifier, Nat), b: (AccountIdentifier, Nat)) : Order.Order {
            return Nat.compare(b.1, a.1);
        };
        let sorted = Array.sort(temp, order);
        let limit_: Nat = if(start + limit > temp.size()) {
            temp.size() - start
        } else {
            limit
        };
        let res = Array.init<(AccountIdentifier, Nat)>(limit_, (ownerAccount, 0));
        for (i in Iter.range(0, limit_ - 1)) {
            res[i] := sorted[i+start];
        };
        return Array.freeze(res);
    };

    public query func getAllowanceSize() : async Nat {
        var size : Nat = 0;
        for ((k, v) in allowances.entries()) {
            size += v.size();
        };
        return size;
    };

    public query func getUserApprovals(who : Text) : async [(AccountIdentifier, Nat)] {
        let _who: AccountIdentifier = switch(Utils.textToPrincipal(who)) { 
            case (?p) {
                PrincipalUtils.toAddress(p);
            }; 
            case (_) {
                if (Utils.isAccountIdentifier(who)) {
                    who;
                } else {
                    return [];
                };
            }
        };
        switch (allowances.get(_who)) {
            case (?allowance_who) {
                return Iter.toArray(allowance_who.entries());
            };
            case (_) {
                return [];
            };
        }
    };

    /*
    * upgrade functions
    */
    system func preupgrade() {
        balanceEntries := Iter.toArray(balances.entries());
        var size : Nat = allowances.size();
        var temp : [var (AccountIdentifier, [(AccountIdentifier, Nat)])] = Array.init<(AccountIdentifier, [(AccountIdentifier, Nat)])>(size, (ownerAccount, []));
        size := 0;
        for ((k, v) in allowances.entries()) {
            temp[size] := (k, Iter.toArray(v.entries()));
            size += 1;
        };
        allowanceEntries := Array.freeze(temp);
    };

    system func postupgrade() {
        balances := HashMap.fromIter<AccountIdentifier, Nat>(balanceEntries.vals(), 1, Text.equal, Text.hash);
        balanceEntries := [];
        for ((k, v) in allowanceEntries.vals()) {
            let allowed_temp = HashMap.fromIter<AccountIdentifier, Nat>(v.vals(), 1, Text.equal, Text.hash);
            allowances.put(k, allowed_temp);
        };
        allowanceEntries := [];
    };
};
