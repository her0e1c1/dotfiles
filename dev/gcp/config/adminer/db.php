<?php

function adminer_object() {
    class AdminerSoftware extends \Adminer\Adminer {
        public function credentials() {
            $host = getenv('MYSQL_HOST') ?: '';
            $port = getenv('MYSQL_PORT') ?: '';

            return array(
                $host . ($host && $port ? ':' . $port : ''),
                getenv('MYSQL_USER') ?: '',
                getenv('MYSQL_PWD') ?: '',
            );
        }

        public function login($login, $password) {
            return true;
        }
    }

    return new AdminerSoftware;
}

include '../adminer.php';
