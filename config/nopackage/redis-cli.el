;;; redis-configs.el --- redis-cli interface         -*- lexical-binding: t; -*-

;; Copyright (C) 2014  Raimon Grau

;; Author: Raimon Grau <raimonster@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Open a redis client buffer with autocompletions

;;; Code:


;;; redis-configs.el ends here

(defun pcomplete-redis-setup ()
      (interactive)
      (set (make-local-variable 'pcomplete-parse-arguments-function)
           'pcomplete-parse-redis-arguments)
      (set (make-local-variable 'pcomplete-default-completion-function)
           'pcomplete-redis-default-completion))

(defun pcomplete-redis-default-completion ()
  (pcomplete-here redis-all-completions))

(defun pcomplete-parse-redis-arguments ()
  (save-excursion
    (let* ((thispt (point))
           (pt (search-backward-regexp "[ \t\n]" nil t))
           (ptt (if pt (+ pt 1) thispt)))
      (list (list "dummy"
                  (buffer-substring-no-properties ptt thispt))
            (point-min) ptt))))

(defvar redis-all-completions '("APPEND" "AUTH" "BGREWRITEAOF" "BGSAVE" "BITCOUNT" "BITOP" "BLPOP" "BRPOP" "BRPOPLPUSH" "CLIENT" "CLIENT" "CLIENT" "CLIENT" "CONFIG" "CONFIG" "CONFIG" "CONFIG" "DBSIZE" "DEBUG" "DEBUG" "DECR" "DECRBY" "DEL" "DISCARD" "DUMP" "ECHO" "EVAL" "EVALSHA" "EXEC" "EXISTS" "EXPIRE" "EXPIREAT" "FLUSHALL" "FLUSHDB" "GET" "GETBIT" "GETRANGE" "GETSET" "HDEL" "HEXISTS" "HGET" "HGETALL" "HINCRBY" "HINCRBYFLOAT" "HKEYS" "HLEN" "HMGET" "HMSET" "HSET" "HSETNX" "HVALS" "INCR" "INCRBY" "INCRBYFLOAT" "INFO" "KEYS" "LASTSAVE" "LINDEX" "LINSERT" "LLEN" "LPOP" "LPUSH" "LPUSHX" "LRANGE" "LREM" "LSET" "LTRIM" "MGET" "MIGRATE" "MONITOR" "MOVE" "MSET" "MSETNX" "MULTI" "OBJECT" "PERSIST" "PEXPIRE" "PEXPIREAT" "PING" "PSETEX" "PSUBSCRIBE" "PUBSUB" "PTTL" "PUBLISH" "PUNSUBSCRIBE" "QUIT" "RANDOMKEY" "RENAME" "RENAMENX" "RESTORE" "RPOP" "RPOPLPUSH" "RPUSH" "RPUSHX" "SADD" "SAVE" "SCARD" "SCRIPT" "SCRIPT" "SCRIPT" "SCRIPT" "SDIFF" "SDIFFSTORE" "SELECT" "SET" "SETBIT" "SETEX" "SETNX" "SETRANGE" "SHUTDOWN" "SINTER" "SINTERSTORE" "SISMEMBER" "SLAVEOF" "SLOWLOG" "SMEMBERS" "SMOVE" "SORT" "SPOP" "SRANDMEMBER" "SREM" "STRLEN" "SUBSCRIBE" "SUNION" "SUNIONSTORE" "SYNC" "TIME" "TTL" "TYPE" "UNSUBSCRIBE" "UNWATCH" "WATCH" "ZADD" "ZCARD" "ZCOUNT" "ZINCRBY" "ZINTERSTORE" "ZRANGE" "ZRANGEBYSCORE" "ZRANK" "ZREM" "ZREMRANGEBYRANK" "ZREMRANGEBYSCORE" "ZREVRANGE" "ZREVRANGEBYSCORE" "ZREVRANK" "ZSCORE" "ZUNIONSTORE" "SCAN" "SSCAN" "HSCAN" "ZSCAN" "append" "auth" "bgrewriteaof" "bgsave" "bitcount" "bitop" "blpop" "brpop" "brpoplpush" "client" "client" "client" "client" "config" "config" "config" "config" "dbsize" "debug" "debug" "decr" "decrby" "del" "discard" "dump" "echo" "eval" "evalsha" "exec" "exists" "expire" "expireat" "flushall" "flushdb" "get" "getbit" "getrange" "getset" "hdel" "hexists" "hget" "hgetall" "hincrby" "hincrbyfloat" "hkeys" "hlen" "hmget" "hmset" "hset" "hsetnx" "hvals" "incr" "incrby" "incrbyfloat" "info" "keys" "lastsave" "lindex" "linsert" "llen" "lpop" "lpush" "lpushx" "lrange" "lrem" "lset" "ltrim" "mget" "migrate" "monitor" "move" "mset" "msetnx" "multi" "object" "persist" "pexpire" "pexpireat" "ping" "psetex" "psubscribe" "pubsub" "pttl" "publish" "punsubscribe" "quit" "randomkey" "rename" "renamenx" "restore" "rpop" "rpoplpush" "rpush" "rpushx" "sadd" "save" "scard" "script" "script" "script" "script" "sdiff" "sdiffstore" "select" "set" "setbit" "setex" "setnx" "setrange" "shutdown" "sinter" "sinterstore" "sismember" "slaveof" "slowlog" "smembers" "smove" "sort" "spop" "srandmember" "srem" "strlen" "subscribe" "sunion" "sunionstore" "sync" "time" "ttl" "type" "unsubscribe" "unwatch" "watch" "zadd" "zcard" "zcount" "zincrby" "zinterstore" "zrange" "zrangebyscore" "zrank" "zrem" "zremrangebyrank" "zremrangebyscore" "zrevrange" "zrevrangebyscore" "zrevrank" "zscore" "zunionstore" "scan" "sscan" "hscan" "zscan" ) )

;;;###autoload
(defun redis-cli ()
  (interactive)
  (with-current-buffer (make-comint-in-buffer "redis" nil "/usr/local/bin/redis-cli")
    (local-set-key (kbd "TAB") 'pcomplete)
    (pcomplete-redis-setup))
  (switch-to-buffer "*redis*"))


(provide 'redis-cli)
