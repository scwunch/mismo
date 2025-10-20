require "./spec_helper"
require "../src/wasm/*"
# require "../src/wasm/code-gen"
require "wasmer"

module Wasm
  def self.add
    <<-WAT
    ;; Basic "add" example.
    ;;
    ;; Eli Bendersky [https://eli.thegreenplace.net]
    ;; This code is in the public domain.
    (module
        ;; add(a, b) returns a+b
        (func $add (export "add") (param $a i32) (param $b i32) (result i32)
            (i32.add (local.get $a) (local.get $b))
        )
    )
    WAT
  end

  def self.add_2
    <<-WAT
    (module
        (func $add     (export "add") (param $a i32) (param $b i32) (result i32)
            local.get $a
            local.get $b
            i32.add
        )
    )
    WAT
  end

  def self.hello_world
    <<-WAT
    (module
      (func (export "hello") (type 0)
        local.get 0
        call 0
      )
      (type 0 (func (param i32)))
    )
    WAT
  end
end

describe Wasmer do
  it "works" do
    # Create a default engine and store
    store = Wasmer.default_engine.new_store

    # Load and compile a WebAssembly module
    # module_ = Wasmer::Module.new store, File.read("path/to/your/file.wasm")
    module_ = Wasmer::Module.new store, Wasmer.wat2wasm(Wasm.add)

    # Create an instance
    instance = Wasmer::Instance.new module_

    # Call WebAssembly functions
    result = instance.function("add").not_nil!.call(40, 2)
    result.should eq(42)
  end

  it "test..." do
    instance = wasm_instance(
      Wasm.add
    )
    result = instance.function("add").not_nil!.call(40, 2)
    result.should eq(42)
  end

  describe "#wat2wasm" do
    it "works" do
      add1 = Wasmer.wat2wasm(Wasm.add)
      add2 = Wasmer.wat2wasm(Wasm.add_2)

      transform = ->(b : UInt8) { sprintf("% 2x", b) }

      # s1 = String.new(add1.map(&transform).to_unsafe, add1.size)
      # s2 = String.new(add2.map(&transform).to_unsafe, add2.size)
      s1 = String.build do |s|
        add1.map(&transform).each do |b|
          s << b
          # s << "."
        end
      end
      s2 = String.build do |s|
        add2.map(&transform).each do |b|
          s << b
          # s << "."
        end
      end
      # p! add1.join(".")
      # p! add2.join(".")
      # p! s1
      # p! s2
      s1.should eq(s2)
    end
  end
end

def wasm_instance(wat : String)
  store = Wasmer.default_engine.new_store
  module_ = Wasmer::Module.new store, Wasmer.wat2wasm(wat)
  instance = Wasmer::Instance.new module_
  instance
end

def wasm_instance(wasm : Wasm::Node, print_out : Bool = false)
  strings = String::Builder.new
  formatter = Wat::Formatter.new(strings)
  wasm.fmt(formatter)
  wat = strings.to_s
  if print_out
    puts "*** \nWAT: \n"
    puts wat
    puts "\n\n***\n\n"
  end
  wasm_instance(wat)
end
  
describe Wasm do
  describe Wat::Formatter do
    it "fromats WAT to text output" do
      formatter = Wat::Formatter.new(STDOUT)
      formatter.println("hello")
      mod = Wat.raw [
        "module",
        ["func", "$add", ["export", "\"add\""],
          ["param", "$a", "i32"],
          ["param", "$b", "i32"],
          ["result", "i32"],
          ["i32.add", ["local.get", "$a"], ["local.get", "$b"]]
        ]
      ]
      mod.fmt(formatter)
      formatter.newline
      # formatter.@io.string.should eq("hello\n")
    end
  end

  describe Wasm::Node do
    it "formats to WAT" do
      mod = Wasm::Module.new([
        Wasm::Function.new(
          Wasm::Identifier.new("add"),
          Wasm::Export.new("add"),
          [
            Wasm::Param.new(
              Wasm::Identifier.new("a"),
              Wasm::Type.new(:i32)
            ),
            Wasm::Param.new(
              Wasm::Identifier.new("b"),
              Wasm::Type.new(:i32)
            )
          ],
          Wasm::Result.new(
            Wasm::Type.new(:i32)
          ),
          body: [
            Wasm::Const.new(:i32, 1_i32).as Wasm::Node,
            Wasm::Const.new(:i32, 2_i32).as Wasm::Node
          ]
        ).as Wasm::Node
      ])
      # p! mod.wat
      # print mod.wat.to_s
      # print '\n'
      mod.fmt(Wat::Formatter.new(STDOUT))
      # p! mod.body.first
      # p! mod.body.first.wat
      # print mod.body.first.to_s
      # print "\n\n***\n\n"
      # mod.body.first.fmt(Wasm::Formatter.new(STDOUT))
    end
  end

  it "formats to WAT and runs" do
    wasm = Wasm::Module.new([
      Wasm::Function.new(
        Wasm::Identifier.new("add"),
        Wasm::Export.new("add"),
        [
          Wasm::Param.new(
            Wasm::Identifier.new("a"),
            Wasm::Type.new(:i32)
          ),
          Wasm::Param.new(
            Wasm::Identifier.new("b"),
            Wasm::Type.new(:i32)
          )
        ],
        Wasm::Result.new(
          Wasm::Type.new(:i32)
        ),
        body: [
          Wasm::Const.new(1_i32).as Wasm::Node,
          Wasm::Const.new(2_i32).as Wasm::Node,
          Wasm::AddAtom.new(:i32).as Wasm::Node
        ]
      ).as Wasm::Node
    ])
    instance = wasm_instance(wasm)
    instance.function("add").not_nil!.call(40, 2).should eq(3)
  end
end

# (func $add (export "add") (param $a i32) (param $b i32) (result i32)
#             (i32.add (local.get $a) (local.get $b))
#         )