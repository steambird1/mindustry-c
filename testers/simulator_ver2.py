import sys
from typing import Dict, List, Any, Union, Optional
from collections import deque

# The code has manual changes merged by steambird
# on 5 Feb.

class MindustryLogicSimulator:
    """
    Mindustry 逻辑代码模拟器
    支持指令: set, op, read, write, jump, end, stop, getlink
    """
    
    def __init__(self, debug: bool = False, strict_mode: bool = False, debug_output_mode: int = 0):
        self.eptr: int = 0
        self.debug = debug
        self.debug_output_mode = debug_output_mode  # 调试输出模式: 0=所有变量, 1=最近10条指令变量, 2=当前指令变量
        self.strict_mode = strict_mode  # 严格模式开关
        self.variables: Dict[str, Any] = {}
        self.memory: Dict[str, List[Any]] = {}
        self.buildings: List[str] = []  # 存储建筑顺序，用于getlink
        self.code: List[str] = []
        self.loop_count: int = 0
        self.pc: int = 0
        self.current_loop: int = 0
        self.running: bool = True
        self.recent_vars = deque(maxlen=10)  # 记录最近10条指令涉及的变量
        self.current_vars = set()  # 当前指令涉及的变量
        
    def parse_value(self, token: str) -> Any:
        """解析一个值，可以是数字、null、变量或字符串"""
        if token == "null":
            return None
        if token == "@counter":
            return self.pc + 1
        if len(token) and token[0] == "@":
            # 手工添加，与其它部分区分
            # (by steambird)
            return f"@@special@{token}"
        if token == "true":
            return True
        if token == "false":
            return False
        
        # 尝试解析为数字
        try:
            # 尝试整数
            return int(token)
        except ValueError:
            try:
                # 尝试浮点数
                return float(token)
            except ValueError:
                # 如果是变量名，则返回变量的值
                if token in self.variables:
                    # 记录当前指令涉及的变量
                    # (Manually added!)
                    self.current_vars.add(token)
                    return self.variables[token]
                # 否则作为字符串（可能是建筑名）
                return token
        
    
    def get_value(self, token: str) -> Any:
        """获取一个值（解析token）"""
        return self.parse_value(token)
    
    def set_variable(self, var_name: str, value: Any):
        """设置变量值"""
        if var_name == "@counter":
            self.pc = int(value) - 1
        self.variables[var_name] = value
        # 记录当前指令涉及的变量
        self.current_vars.add(var_name)
    
    def read_memory(self, mem_name: str, index: int) -> Any:
        """从内存建筑读取值"""
        if mem_name not in self.memory:
            raise ValueError(f"内存建筑 '{mem_name}' 未初始化")
        
        if index < 0 or index >= len(self.memory[mem_name]):
            return None
        
        return self.memory[mem_name][index]
    
    def write_memory(self, mem_name: str, index: int, value: Any):
        """向内存建筑写入值"""
        if mem_name not in self.memory:
            raise ValueError(f"内存建筑 '{mem_name}' 未初始化")
        
        if 0 <= index < len(self.memory[mem_name]):
            # 严格模式检查
            if self.strict_mode:
                if not isinstance(value, (int, float)):
                    raise ValueError(f"严格模式下不允许向内存写入非数字值: {value}")
            else:
                # 非严格模式：将值转换为数字
                if isinstance(value, (int, float)):
                    pass  # 已经是数字，无需转换
                elif value is None:
                    value = 0
                elif isinstance(value, str) and value:
                    # 非空字符串视为1
                    value = 1
                elif isinstance(value, str) and not value:
                    # 空字符串视为0
                    value = 0
                elif value:
                    # 其他真值视为1
                    value = 1
                else:
                    # 其他假值视为0
                    value = 0
            
            self.memory[mem_name][index] = value
    
    def do_operation(self, op: str, val1: Any, val2: Any) -> Any:
        """执行运算操作"""
        # 处理 null 值
        if val1 is None or val2 is None:
            if op in ["equal", "notEqual", "strictEqual"]:
                # 比较操作中 null 的处理
                if op == "equal":
                    return 1 if val1 == val2 else 0
                elif op == "notEqual":
                    return 1 if val1 != val2 else 0
                elif op == "strictEqual":
                    return 1 if val1 is val2 else 0
            # 其他操作中遇到 null 返回 0
            return 0
        
        if op == "add":
            # 数字相加或字符串连接
            if isinstance(val1, (int, float)) and isinstance(val2, (int, float)):
                return val1 + val2
            else:
                return str(val1) + str(val2)
        elif op == "sub":
            if isinstance(val1, (int, float)) and isinstance(val2, (int, float)):
                return val1 - val2
            return 0
        elif op == "mul":
            if isinstance(val1, (int, float)) and isinstance(val2, (int, float)):
                return val1 * val2
            return 0
        elif op == "div":
            if isinstance(val1, (int, float)) and isinstance(val2, (int, float)):
                if val2 == 0:
                    return 0
                return val1 / val2
            return 0
        elif op == "mod":
            if isinstance(val1, (int, float)) and isinstance(val2, (int, float)):
                if val2 == 0:
                    return 0
                return val1 % val2
            return 0
        elif op == "equal":
            return 1 if val1 == val2 else 0
        elif op == "notEqual":
            return 1 if val1 != val2 else 0
        elif op == "lessThan":
            if isinstance(val1, (int, float)) and isinstance(val2, (int, float)):
                return 1 if val1 < val2 else 0
            return 0
        elif op == "lessThanEq":
            if isinstance(val1, (int, float)) and isinstance(val2, (int, float)):
                return 1 if val1 <= val2 else 0
            return 0
        elif op == "greaterThan":
            if isinstance(val1, (int, float)) and isinstance(val2, (int, float)):
                return 1 if val1 > val2 else 0
            return 0
        elif op == "greaterThanEq":
            if isinstance(val1, (int, float)) and isinstance(val2, (int, float)):
                return 1 if val1 >= val2 else 0
            return 0
        elif op == "strictEqual":
            # 严格相等，需要类型和值都相同
            if type(val1) == type(val2) and val1 == val2:
                return 1
            return 0
        else:
            raise ValueError(f"不支持的运算符: {op}")
    
    def numeric_convert(self, val) -> float:
        """将值转换为数字表示（用于比较）"""
        if isinstance(val, (int, float)):
            return float(val)
        elif val is None:
            return 0.0
        elif val == True:
            return 1.0
        elif val == False:
            return 0.0
        elif isinstance(val, str) and val:
            return 1.0  # 非空字符串视为1
        elif isinstance(val, str) and not val:
            return 0.0  # 空字符串视为0
        else:
            return 1.0 if bool(val) else 0.0

    def do_jump_condition(self, cond: str, val1: Any, val2: Any) -> bool:
        """判断跳转条件是否满足"""
        if cond == "always":
            return True
        elif cond == "equal":
            return val1 == val2
        elif cond == "notEqual":
            return val1 != val2
        elif cond == "lessThan":
            return self.numeric_convert(val1) < self.numeric_convert(val2)
        elif cond == "lessThanEq":
            return self.numeric_convert(val1) <= self.numeric_convert(val2)
        elif cond == "greaterThan":
            return self.numeric_convert(val1) > self.numeric_convert(val2)
        elif cond == "greaterThanEq":
            return self.numeric_convert(val1) >= self.numeric_convert(val2)
        elif cond == "strictEqual":
            return type(val1) == type(val2) and val1 == val2
        else:
            raise ValueError(f"不支持的跳转条件: {cond}")
    
    def parse_input(self, input_str: str):
        """解析输入字符串"""
        lines = [line.strip() for line in input_str.strip().splitlines() if line.strip()]
        
        # 找到初始化结束标记
        init_end_index = -1
        for i, line in enumerate(lines):
            if line.startswith('#'):
                init_end_index = i
                break
        
        if init_end_index == -1:
            raise ValueError("未找到初始化结束标记 (#)")
        
        # 解析循环次数
        loop_line = lines[init_end_index]
        loop_parts = loop_line.split()
        if len(loop_parts) < 2:
            raise ValueError("循环次数未指定")
        self.loop_count = int(loop_parts[1])
        
        # 解析初始化部分
        self.buildings = []  # 清空建筑列表
        for i in range(init_end_index):
            line = lines[i]
            if ':=' in line:
                # 变量初始化
                var_parts = line.split(':=', 1)
                var_name = var_parts[0].strip()
                value_str = var_parts[1].strip()
                value = self.parse_value(value_str)
                self.variables[var_name] = value
                
            elif ':' in line and '{' in line and '}' in line:
                # 内存初始化
                mem_parts = line.split(':', 1)
                mem_name = mem_parts[0].strip()
                mem_list_str = mem_parts[1].strip()
                
                # 提取花括号内的内容
                start = mem_list_str.find('{')
                end = mem_list_str.find('}')
                if start == -1 or end == -1:
                    raise ValueError(f"内存初始化格式错误: {line}")
                
                elements_str = mem_list_str[start+1:end]
                if not elements_str.strip():
                    elements = []
                else:
                    elements = [elem.strip() for elem in elements_str.split(',')]
                
                # 解析每个元素
                mem_values = []
                for elem in elements:
                    mem_values.append(self.parse_value(elem))
                
                self.memory[mem_name] = mem_values
                # 将建筑添加到建筑列表中
                self.buildings.append(mem_name)
        
        # 解析代码部分
        code_start = init_end_index + 1
        for i in range(code_start, len(lines)):
            line = lines[i]
            if line == '@':
                break
            self.code.append(line)
    
    def get_debug_variables(self):
        """根据调试输出模式获取要显示的变量"""
        if self.debug_output_mode == 0:
            # 模式0: 输出所有变量
            return self.variables
        elif self.debug_output_mode == 1:
            # 模式1: 输出最近10条指令涉及到的变量
            recent_vars = set()
            for var_set in self.recent_vars:
                recent_vars.update(var_set)
            return {var: self.variables[var] for var in recent_vars if var in self.variables}
        elif self.debug_output_mode == 2:
            # 模式2: 输出当前指令涉及到的变量
            return {var: self.variables[var] for var in self.current_vars if var in self.variables}
        else:
            return self.variables
    
    def execute_instruction(self, line: str) -> Optional[str]:
        """执行单条指令，返回指令类型或None（如果是end/stop）"""
        self.current_vars = set()
        self.eptr += 1
        parts = line.split()
        if not parts:
            return None
        
        instr = parts[0]
        result_var = None
        
        if instr == "set":
            # set 变量 值
            var_name = parts[1]
            value = self.get_value(parts[2])
            self.set_variable(var_name, value)
            
        elif instr == "op":
            # op 运算符 结果变量 值1 值2
            operator = parts[1]
            result_var = parts[2]
            val1 = self.get_value(parts[3])
            val2 = self.get_value(parts[4])
            result = self.do_operation(operator, val1, val2)
            self.set_variable(result_var, result)
            
        elif instr == "read":
            # read 结果变量 存储建筑 索引
            result_var = parts[1]
            mem_name = self.get_value(parts[2])
            index = self.get_value(parts[3])
            
            if not isinstance(mem_name, str):
                mem_name = str(mem_name)
            
            if not isinstance(index, (int, float)):
                # 尝试转换索引为整数
                try:
                    index = int(index)
                except:
                    index = 0
            
            value = self.read_memory(mem_name, int(index))
            self.set_variable(result_var, value)
            
        elif instr == "write":
            # write 值 存储建筑 索引
            value = self.get_value(parts[1])
            mem_name = self.get_value(parts[2])
            index = self.get_value(parts[3])
            
            if not isinstance(mem_name, str):
                mem_name = str(mem_name)
            
            if not isinstance(index, (int, float)):
                # 尝试转换索引为整数
                try:
                    index = int(index)
                except:
                    index = 0
            
            self.write_memory(mem_name, int(index), value)
            
        elif instr == "getlink":
            # getlink 目标变量 编号
            result_var = parts[1]
            index = self.get_value(parts[2])
            
            if not isinstance(index, (int, float)):
                # 尝试转换索引为整数
                try:
                    index = int(index)
                except:
                    index = 0
            
            # 获取建筑
            if 0 <= index < len(self.buildings):
                building_name = self.buildings[index]
                self.set_variable(result_var, building_name)
            else:
                self.set_variable(result_var, None)
        elif instr == "sensor":
            # (手动添加) sensor 目标变量 建筑 参量
            result_var = parts[1]
            target_building = self.get_value(parts[2])
            target_type = self.get_value(parts[3])
            result = None

            if target_type == "@@special@@type":
                if "cell" in target_building:
                    result = "@@special@@memory-cell"
                elif "bank" in target_building:
                    result = "@@special@@memory-bank"
            
            self.set_variable(result_var, result)

        elif instr == "print":
            print("程序输出***：", self.get_value(parts[1]))
        elif instr == "printflush":
            print("程序输出缓冲***")
        elif instr == "wait":
            print("程序等待***")
        elif instr == "jump":
            # jump 行号 条件类型 值1 值2
            target_line = int(parts[1])
            condition = parts[2]
            val1 = self.get_value(parts[3])
            val2 = self.get_value(parts[4])
            
            if self.do_jump_condition(condition, val1, val2):
                self.pc = target_line
            else:
                self.pc += 1
                
        elif instr == "end":
            # 结束当前循环
            self.current_loop += 1
            self.pc = 0
            return "end"
            
        elif instr == "stop":
            # 停止执行
            self.running = False
            return "stop"
        
        else:
            raise ValueError(f"未知指令: {instr}")
        
        if instr != "jump" and result_var != "@counter":
            self.pc += 1
            
        return instr
    
    def run(self):
        """运行模拟器"""
        self.pc = 0
        self.current_loop = 0
        self.running = True
        
        if self.debug:
            print("=== 初始化状态 ===")
            print(f"变量: {self.variables}")
            print(f"内存: {self.memory}")
            print(f"建筑顺序: {self.buildings}")
            print(f"代码行数: {len(self.code)}")
            print(f"循环次数: {self.loop_count}")
            print(f"严格模式: {self.strict_mode}")
            print(f"调试状态：{self.debug_output_mode}")
            print("=" * 50)
        
        while self.running and self.current_loop < self.loop_count:
            if self.pc >= len(self.code):
                # 代码执行完毕，开始下一次循环
                self.current_loop += 1
                self.pc = 0
                continue
            
            line = self.code[self.pc]
            
            if self.debug:
                print(f"[循环 {self.current_loop+1}/{self.loop_count}, 行 {self.pc}] 执行: {line}")

            try:
                result = self.execute_instruction(line)
                
                if self.debug:
                    # 将当前指令涉及的变量添加到最近变量记录中
                    if self.current_vars:
                        self.recent_vars.append(self.current_vars.copy())
                    
                    # 根据调试输出模式获取要显示的变量
                    debug_vars = self.get_debug_variables()
                    print(f"  变量状态: {debug_vars}")
                    print("-" * 30)
                
                if result in ["end", "stop"]:
                    if result == "stop":
                        break
                    # end 指令已处理，继续下一循环
                    continue
                    
            except Exception as e:
                print(f"执行错误 (行 {self.pc}): {e}")
                break
        
        if self.debug:
            print("=" * 50)
            print("=== 执行完成 ===")
        
        return self.variables
    
    def get_results(self) -> Dict[str, Any]:
        """获取最终变量结果"""
        return self.variables

def test_example():
    """测试示例代码"""
    input_code = """initVar := 100
cell1 : {cell2,null,64}
cell2 : {null,cell1,64}
# 1
read block cell1 0
read result block 2
op add final initVar result
@"""
    
    print("测试示例代码:")
    print(input_code)
    print("\n" + "="*50 + "\n")
    
    # 非严格模式
    simulator = MindustryLogicSimulator(debug=False, strict_mode=False)
    simulator.parse_input(input_code)
    results = simulator.run()
    
    print("变量结果:")
    for var_name, value in sorted(results.items()):
        print(f"{var_name} = {value}")
    
    print("\n" + "="*50 + "\n")

def test_getlink():
    """测试getlink指令"""
    input_code = """cell1 : {1,2,3}
cell2 : {4,5,6}
cell3 : {7,8,9}
# 1
getlink first 0
getlink second 1
getlink third 2
getlink invalid 5
@"""
    
    print("测试getlink指令:")
    print(input_code)
    print("\n" + "="*50 + "\n")
    
    simulator = MindustryLogicSimulator(debug=True, strict_mode=False)
    simulator.parse_input(input_code)
    results = simulator.run()
    
    print("\n最终变量结果:")
    for var_name, value in sorted(results.items()):
        print(f"{var_name} = {value}")

def test_strict_mode():
    """测试严格模式"""
    input_code = """value := "hello"
cell1 : {0,0,0}
# 1
write value cell1 0
@"""
    
    print("测试严格模式:")
    print(input_code)
    print("\n" + "="*50 + "\n")
    
    # 非严格模式
    print("非严格模式执行:")
    simulator1 = MindustryLogicSimulator(debug=True, strict_mode=False)
    simulator1.parse_input(input_code)
    try:
        results1 = simulator1.run()
        print("\n非严格模式执行成功")
        print(f"内存状态: {simulator1.memory}")
    except Exception as e:
        print(f"非严格模式执行错误: {e}")
    
    print("\n" + "-"*30 + "\n")
    
    # 严格模式
    print("严格模式执行:")
    simulator2 = MindustryLogicSimulator(debug=True, strict_mode=True)
    simulator2.parse_input(input_code)
    try:
        results2 = simulator2.run()
        print("\n严格模式执行成功")
    except Exception as e:
        print(f"严格模式执行错误: {e}")

if __name__ == "__main__":
    debugging = input("debug (Y/N)?").strip().lower() == 'y'
    strict = input("strict mode (Y/N)?").strip().lower() == 'y'
    if debugging:
        print("调试输出模式: 0=所有变量, 1=最近10条指令变量, 2=当前指令变量")
        try:
            debug_mode = int(input("选择调试输出模式 (0/1/2): "))
        except:
            debug_mode = 0
    else:
        debug_mode = 0
    print("Input code:")
    code = []
    res = ""
    while res != "@":
        res = input()
        code.append(res)
    simu = MindustryLogicSimulator(debug=debugging, strict_mode=strict, debug_output_mode=debug_mode)
    simu.parse_input("\n".join(code))
    results = simu.run()
    
    print("\n最终变量结果:")
    for var_name, value in sorted(results.items()):
        print(f"{var_name} = {value}")
    
    if simu.memory:
        print(f"\n内存状态: {simu.memory}")