-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2025 hyperpolymath
--
-- NoteG Engine - Ada/SPARK Core
-- Mill-based synthesis engine for static site generation

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Containers.Vectors;
with Ada.Containers.Hashed_Maps;

package NoteG_Engine with SPARK_Mode is

   -- Version information
   Version : constant String := "0.1.0";

   -- Maximum limits for SPARK verification
   Max_Variables : constant := 1024;
   Max_Template_Depth : constant := 32;
   Max_Content_Size : constant := 1_048_576; -- 1MB

   -- Variable store types
   type Variable_Kind is (Text, Number, Boolean_Val, List, Object);

   type Variable_Value (Kind : Variable_Kind := Text) is record
      case Kind is
         when Text =>
            Text_Val : Unbounded_String;
         when Number =>
            Num_Val : Long_Float;
         when Boolean_Val =>
            Bool_Val : Boolean;
         when List | Object =>
            -- Handled via reference
            Ref_Id : Natural;
      end case;
   end record;

   -- Operation card for mill-based synthesis
   type Operation_Kind is (
      Load_Variable,
      Store_Variable,
      Template_Substitute,
      Content_Transform,
      Output_Generate,
      Conditional_Branch,
      Loop_Start,
      Loop_End
   );

   type Operation_Card is record
      Op : Operation_Kind;
      Operand_1 : Unbounded_String;
      Operand_2 : Unbounded_String;
      Result_Ref : Natural;
   end record;

   -- Mill state
   type Mill_State is record
      Current_Card : Natural := 0;
      Stack_Depth : Natural := 0;
      Error_Flag : Boolean := False;
      Error_Message : Unbounded_String;
   end record;

   -- Core operations
   procedure Initialize
     with Global => null,
          Post => True;

   procedure Load_Template (
      Path : in String;
      Success : out Boolean)
     with Pre => Path'Length > 0 and Path'Length <= 4096;

   procedure Process_Content (
      Input : in String;
      Output : out Unbounded_String;
      Success : out Boolean)
     with Pre => Input'Length <= Max_Content_Size;

   procedure Execute_Card (
      Card : in Operation_Card;
      State : in out Mill_State;
      Success : out Boolean);

   -- Variable store operations
   procedure Set_Variable (
      Name : in String;
      Value : in Variable_Value;
      Success : out Boolean)
     with Pre => Name'Length > 0 and Name'Length <= 256;

   procedure Get_Variable (
      Name : in String;
      Value : out Variable_Value;
      Found : out Boolean)
     with Pre => Name'Length > 0 and Name'Length <= 256;

   procedure Clear_Variables;

   -- Template operations
   function Substitute_Variables (
      Template : String) return Unbounded_String
     with Pre => Template'Length <= Max_Content_Size;

   -- Verification predicates
   function Is_Valid_Template (Template : String) return Boolean;
   function Is_Valid_Variable_Name (Name : String) return Boolean;

private
   -- Internal state (hidden from SPARK verification of callers)
   Current_Mill_State : Mill_State;

end NoteG_Engine;
