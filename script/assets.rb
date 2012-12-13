#!/usr/bin/env ruby

require "rubygems"
require "listen"

callback = Proc.new { puts `make assets` }

Listen.to("assets/stylesheets", "assets/javascripts")
  .ignore(/-min\.(css|js)$/)
  .change(&callback)
  .start
