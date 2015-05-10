#!/bin/bash
sudo -u postgres psql -c "CREATE ROLE test WITH LOGIN CREATEDB PASSWORD 'test';"
sudo -u postgres psql -c "CREATE DATABASE perscotty;"
