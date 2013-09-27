#!/usr/bin/env python3
import os
import sys
import argparse
import sqlalchemy as sql
import collections
import tempfile
from getpass import getpass
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker
from Crypto.Cipher import AES


Base = declarative_base()
class Account(Base):
    __tablename__ = "account"

    id = sql.Column(sql.Integer, primary_key=True)
    account = sql.Column(sql.String, nullable=True)
    password = sql.Column(sql.String, nullable=False)
    name = sql.Column(sql.String, nullable=True)

    def __init__(self, account, password, name):
        self.account = account
        self.password = password
        self.name = name

class AccountHandler(object):

    def __init__(self, key):
        self.format = "%32s"
        self.aes = AES.new(self.format % key)

    def encrypt(self, account, password, aes=None):
        if aes is None:
            aes = self.aes
        return tuple(aes.encrypt(self.format % x) for x in [account, password])

    def decrypt(self, account, password, aes=None):
        if aes is None:
            aes = self.aes
        try:
            return tuple(aes.decrypt(x).decode() for x in [account, password])
        except UnicodeDecodeError:
            return tuple(aes.decrypt(x) for x in [account, password])

    # make instances
    def _create(self, account, password, name):
        encrypted_account, encrypted_password = self.encrypt(account, password)
        a = Account(account=encrypted_account, password=encrypted_password, name=name)
        if not self.exists(account):
            return a
        else:
            raise ValueError("%s already exists" % account)

    def _delete(self, id, account=None):
        one = session.query(Account).filter_by(id=id).one()
        return one

    def _change_master_key(self, new_aes):
        for a in self._all_accounts():
            # get account info
            old_account, old_password = self.decrypt(a.account, a.password)
            # set new encrypted account info 
            a.account, a.password = self.encrypt(old_account, old_password, new_aes)
            yield a
        self.aes = new_aes

    # session
    def _all_accounts(self):
        return session.query(Account).all()

    def _commit(self, accounts, method="add"):
        if not hasattr(session, method) or accounts is None:
            return # commit nothing

        func = getattr(session, method)
        if isinstance(accounts, collections.Iterable):
            for a in accounts:
                func(a)
        else:
            func(accounts)
        session.commit()
        
    def _session_add(self, accounts):
        self._commit(accounts, "add")

    def _session_delete(self, accounts):
        self._commit(accounts, "delete")

    # api
    def exists(self, target_account):
        if self.search_account(target_account) is None:
            return False
        else:
            return True
        
    def search_account(self, target_account):
        """
        find the encrypted account or the decrypted account by a ``target_account`` key.
        In an either way, return id, **decrypted account**, **decrypted password**.
        """
        for a in self._all_accounts():
            id = a.id
            account, password = self.decrypt(a.account, a.password)
            if account == target_account or a.account == target_account:
                return id, account, password

    def show_all(self):
        """
        print information about all acounts.
        """
        buff = ["id, account, password, name"]
        buff.append("-" * 100)
        for a in self._all_accounts():
            id = a.id
            name = a.name
            account, password = self.decrypt(a.account, a.password)
            buff.append("{0}, {1}, {2}, {3}".format(id, account, password, name))
        
        with tempfile.NamedTemporaryFile("w+t") as tf:
            tf.write("\n".join(buff))
            tf.flush()
            os.system("less {}".format(tf.name))


    def create_account(self, account=None, password=None, name=None):
        """if there is no account on DB, then make it. """
        # intput
        if account is None:
            account = input("account> ")
        if password is None:
            password = getpass("password> ")
        if name is None:
            name = input("name> ")
        self._session_add(self._create(account, password, name))

    def change_password(self, target_account=None, old_password=None, new_password=None):
        """change a password of the target_account into a new password"""
        if target_account is None:
            target_account = input("account> ")
        if old_password is None:
            old_password = getpass("old password> ")
        if new_password is None:
            new_password = getpass("new password> ")

        if not self.exists(target_account):
            raise ValueError("%s doesn't exits" % target_account) 

        id, account, password, name = self.search_account(target_account)
        if password == old_password:
            self._session_delete(self._delete(id))
            self._session_add(self._create(account, new_password, name))
        else:
            print("%s doesn't match the registered password" % old_password)

    def change_master_key(self):
        new_key = getpass("new master key> ")
        new_aes = AES.new(self.format % new_key)
        self._session_add(self._change_master_key(new_aes))

    def delete_all_accounts(self):
        if ask_yes_or_no("Delete all accounts. Are you sure?[y/n]"):
            self._session_delete(self._all_accounts())

    def delete_by_id(self):
        id = int(input("delete id> "))
        if ask_yes_or_no("Delete %d account. Are you sure?[y/n]" % id):
            self._session_delete(self._delete(id))


def ask_yes_or_no(prompt="yes or no"):
    ans = input(prompt)
    if ans.lower() in ("y", "yes", "1", "true", "t"):
        return True
    else:
        return False
    

def make_session():
    from myconfig import MYCONFIG
    path = "sqlite:////" + MYCONFIG.values("mine.db")
    engine = sql.create_engine(path)
    Account.metadata.create_all(engine)
    Session = sessionmaker(bind=engine)
    session = Session()
    return session


def main():
    p = argparse.ArgumentParser()
    p.add_argument("-c", "--create-account", action="store_true")
    p.add_argument("-s", "--show_all", action="store_true")
    p.add_argument("-m", "--change-master-key", action="store_true")
    p.add_argument("-p", "--change-password", action="store_true")
    p.add_argument("-d", "--delete-by-id", action="store_true")
    p.add_argument("--delete-all-accounts", action="store_true")
    args = p.parse_args()

    key = getpass("master key> ")
    ac = AccountHandler(key)

    for method in [x for x in dir(ac) 
                   if not x.startswith("_") and callable(getattr(ac, x))]:
        func = getattr(ac, method)
        if hasattr(args, method) and getattr(args, method) == True:
            func()


session = make_session()
if __name__ == "__main__":
    main()
