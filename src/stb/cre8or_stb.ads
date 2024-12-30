------------------------------------------------------------------------------------------------------------------------
--  Copyright 2024 Cre8or                                                                                             --
--                                                                                                                    --
--  Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance    --
--  with the License. You may obtain a copy of the License at                                                         --
--                                                                                                                    --
--     http://www.apache.org/licenses/LICENSE-2.0                                                                     --
--                                                                                                                    --
--  Unless required by applicable law or agreed to in writing, software distributed under the License is distributed  --
--  on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.                     --
--  See the License for the specific language governing permissions and limitations under the License.                --
------------------------------------------------------------------------------------------------------------------------





with Interfaces.C;

with System;



pragma Elaborate_All (Interfaces.C);

pragma Elaborate_All (System);





package Cre8or_stb is



	-- Types (1 / 2)
	subtype T_Integer is Interfaces.C.int;
	subtype T_Natural is T_Integer range 0 .. T_Integer'Last;

	subtype T_Address is System.Address;



	-- Constants
	C_Null : constant T_Address := System.Null_Address;



	-- Types (2 / 2)
	type T_Data_Chunk is record
		m_Address : T_Address := C_Null;
		m_Length  : T_Natural := 0; -- In bytes
	end record;



private



	-- Use clauses
	use type T_Integer;

	use type T_Address;



end Cre8or_stb;
