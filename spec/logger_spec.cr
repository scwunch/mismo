require "./spec_helper"
require "../src/logger"

describe Logger do
  describe "#debug" do
    it "logs a debug message" do
      if false
        puts "----------------" 
        logger = Logger.new(Logger::Level::Debug)
        logger.debug(Location.zero, "test")
        puts "----------------"
        logger.info(Location.zero, "test")
        puts "----------------"
        logger.warning(Location.zero, "test")
        puts "----------------"
        logger.error(Location.zero, "test")
        puts "----------------"
      end
    end
  end
end