#define UNICODE 1 
#define _UNICODE 1 


#include <assert.h>
#include <stdio.h> 
#include <stdlib.h> 
#include <string.h> 
#include <malloc.h> 

#include <windows.h> 
#include <winperf.h> 
#include <tchar.h> 

#define RESERVED        0L       // reserved argument in fn. calls 
#define INITIAL_SIZE    40960L   // initial perf data buff size 
#define EXTEND_SIZE     4096L    // extend size of perf data buff 
#define LINE_LENGTH     80       // display line length 
#define SLEEP_TIME		1000	 // Time interval between load polls.
#define SAMPLE			5		 // How many samples do we keep


const LPTSTR NamesKey = TEXT("SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion\\Perflib"); 
const LPTSTR DefaultLangId = TEXT("009"); 
const LPTSTR Counters = TEXT("Counter "); 
const LPTSTR Help = TEXT("Explain "); 
const LPTSTR LastHelp = TEXT("Last Help"); 
const LPTSTR LastCounter = TEXT("Last Counter"); 
const LPTSTR Slash = TEXT("\\"); 
const LPTSTR Global = TEXT("Global"); 
const LPTSTR ObjectName = TEXT("System");
const LPTSTR CounterName = TEXT("% Total Processor Time");


HANDLE Mutex;			// Mutex to protect the load variable.
int Load[SAMPLE];		// Keeps the current load.
int CurLoadIndex = 0;	// Points to next available slot.


/*++ 
 
StringToInt 
 
    Returns the decimal interpretation of the string in the InStr 
    argument. Works for either Unicode or ANSI strings. 
 
Arguments: 
 
    InStr 
        string to translate 
 
    pdwOutInt 
        base 10 version of the number in the InStr argument 
 
Return Value: 
 
    TRUE if conversion was successful 
    FALSE if not 
 
--*/ 
BOOL 
StringToInt ( 
    IN  LPTSTR  InStr, 
    OUT PDWORD  pdwOutInt 
) 
{ 
 
    // check for NULL pointers passed to function 
 
 
    if ((!InStr) || (!pdwOutInt)) { 
        SetLastError (ERROR_BAD_ARGUMENTS); 
        return FALSE; 
    } 
 
    if ( (_stscanf (InStr, TEXT(" %d"), pdwOutInt)) == 1) { 
        return TRUE; 
    } else { 
        return FALSE; 
    } 
} // StringToInt 




/*++ 
 
BuildNameTable 
 
    Caches the counter names and explain text to accelerate name lookups 
    for display. 
 
Arguments: 
 
    hKeyRegistry 
        Handle to an open registry (this can be local or remote.) and 
        is the value returned by RegConnectRegistry or a default key. 
 
    lpszLangId 
            The unicode id of the language to look up. (default is 009) 
 
    pdwLastItem 
            The last array element 
 
Return Value: 
 
    pointer to an allocated table. 
    (the caller must free it when finished!) 
 
    the table is an array of pointers to zero terminated TEXT strings. 
 
    A NULL pointer is returned if an error occured. (error value is 
    available using the GetLastError function). 
 
    The structure of the buffer returned is: 
 
        Array of pointers to zero terminated strings consisting of 
            pdwLastItem elements 
 
        MULTI_SZ string containing counter id's and names returned from 
            registry for the specified language 
 
        MULTI_SZ string containing explain text id's 
            and explain text strings 
            as returned by the registry for the specified language 
 
    The structures listed above are contiguous so that they may be freed 
    by a single "free" call when finished with them, however only the 
    array elements are intended to be used. 
 
--*/ 
LPTSTR 
*BuildNameTable( 
    HKEY    hKeyRegistry,   // handle to registry db with counter names 
    LPTSTR  lpszLangId,     // unicode value of Language subkey 
    PDWORD  pdwLastItem     // size of array in elements 
) 
{ 
 
    LPTSTR  *lpReturnValue;     // returned pointer to buffer 
 
    LPTSTR  *lpCounterId;       // 
    LPTSTR  lpCounterNames;  // pointer to Names buffer returned by reg. 
    LPTSTR  lpHelpText ;     // pointet to exlpain buffer returned by reg. 
 
    LPTSTR  lpThisName;         // working pointer 
 
    BOOL    bStatus;            // return status from TRUE/FALSE fn. calls 
    LONG    lWin32Status;       // return status from fn. calls 
 
 
    DWORD   dwHelpItems;        // number of explain text items 
    DWORD   dwCounterItems;     // number of counter text items 
    DWORD   dwValueType;        // value type of buffer returned by reg. 
    DWORD   dwArraySize;        // size of pointer array in bytes 
    DWORD   dwBufferSize;       // size of total buffer in bytes 
    DWORD   dwCounterSize;      // size of counter text buffer in bytes 
    DWORD   dwHelpSize;         // size of help text buffer in bytes 
    DWORD   dwThisCounter;      // working counter 
 
 
    DWORD   dwLastId;           // largest ID value used by explain/counter text 
 
    HKEY    hKeyValue;          // handle to Perflib entry in registry 
 
    TCHAR   CounterNameBuffer[LINE_LENGTH]; // buffer conatining Counter query 
    TCHAR   HelpNameBuffer[LINE_LENGTH]; // buffer conatining Help query 
    BOOL    bClosePerformanceKey; // flag for closing HKEY_PERFORMANCE_DATA key 
 
 
    //initialize data 
 
    lpReturnValue = NULL; 
    hKeyValue = NULL; 
    bClosePerformanceKey = FALSE; 
 
    // check for null arguments and insert defaults if necessary 
 
    if (!lpszLangId) { 
        lpszLangId = DefaultLangId; 
    } 
 
    // open handle to Perflib key in registry to get number of items 
    // used in order to compute array and buffer sizes 
 
    lWin32Status = RegOpenKeyEx ( 
        hKeyRegistry, 
        NamesKey, 
        RESERVED, 
        KEY_READ, 
        &hKeyValue); 
 
    // on error, free buffers, close keys and return NULL pointer 
 
    if (lWin32Status != ERROR_SUCCESS) { 
        goto BNT_BAILOUT; 
    } 
 
    // query registry to get number of Explain text items 
 
    dwBufferSize = sizeof (dwHelpItems); 
    lWin32Status = RegQueryValueEx ( 
        hKeyValue, 
        LastHelp, 
        RESERVED, 
        &dwValueType, 
        (LPBYTE)&dwHelpItems, 
        &dwBufferSize); 
 
    if ((lWin32Status != ERROR_SUCCESS) || (dwValueType != REG_DWORD)) { 
        goto BNT_BAILOUT; 
    } 
 
    // query registry to get number of counter and object name items 
 
    dwBufferSize = sizeof (dwCounterItems); 
    lWin32Status = RegQueryValueEx ( 
        hKeyValue, 
        LastCounter, 
        RESERVED, 
        &dwValueType, 
        (LPBYTE)&dwCounterItems, 
        &dwBufferSize); 
 
    if ((lWin32Status != ERROR_SUCCESS) || (dwValueType != REG_DWORD)) { 
        goto BNT_BAILOUT; 
    } 
 
    // use the greater of Help items or Counter Items to size array 
 
    if (dwHelpItems >= dwCounterItems) { 
        dwLastId = dwHelpItems; 
    } else { 
        dwLastId = dwCounterItems; 
    } 
 
    // array size is # of elements (+ 1, since names are "1" based) 
    // times the size of a pointer 
 
    dwArraySize = (dwLastId + 1) * sizeof(LPTSTR); 
 
    // build the Query strings for the specified language ID 
 
    lstrcpy (CounterNameBuffer, Counters); 
    lstrcat (CounterNameBuffer, lpszLangId); 
 
    lstrcpy (HelpNameBuffer, Help); 
    lstrcat (HelpNameBuffer, lpszLangId); 
 
    // get size of counter names 
 
    dwBufferSize = 0; 
    lWin32Status = RegQueryValueEx ( 
        HKEY_PERFORMANCE_DATA, 
        CounterNameBuffer, 
        RESERVED, 
        &dwValueType, 
        NULL, 
        &dwBufferSize); 
 
    if (lWin32Status != ERROR_SUCCESS) goto BNT_BAILOUT; 
 
    bClosePerformanceKey = TRUE; 
    dwCounterSize = dwBufferSize; 
 
    // get size of help text 
 
    dwBufferSize = 0; 
    lWin32Status = RegQueryValueEx ( 
        HKEY_PERFORMANCE_DATA, 
        HelpNameBuffer, 
        RESERVED, 
        &dwValueType, 
        NULL, 
        &dwBufferSize); 
 
 
    if (lWin32Status != ERROR_SUCCESS) goto BNT_BAILOUT; 
 
    dwHelpSize = dwBufferSize; 
 
    // allocate buffer with room for pointer array, counter name 
    // strings and help name strings 
 
    lpReturnValue = malloc (dwArraySize + dwCounterSize + dwHelpSize); 
 
    if (!lpReturnValue) { 
        lWin32Status = ERROR_OUTOFMEMORY; 
        goto BNT_BAILOUT; 
    } 
 
    // initialize pointers into buffer 
 
    lpCounterId = lpReturnValue; 
    lpCounterNames = (LPTSTR)((LPBYTE)lpCounterId + dwArraySize); 
 
    lpHelpText = (LPTSTR)((LPBYTE)lpCounterNames + dwCounterSize); 
 
    // initialize the pointers to NULL 
    for (dwBufferSize = 0; dwBufferSize <= dwLastId; dwBufferSize++) { 
        lpCounterId[dwBufferSize] = NULL; 
    } 
 
    // read counter names into buffer. Counter names will be stored as 
    // a MULTI_SZ string in the format of "###" "Name" 
 
    dwBufferSize = dwCounterSize; 
    lWin32Status = RegQueryValueEx ( 
        HKEY_PERFORMANCE_DATA, 
        CounterNameBuffer, 
        RESERVED, 
        &dwValueType, 
        (LPVOID)lpCounterNames, 
        &dwBufferSize); 
 
    if (lWin32Status != ERROR_SUCCESS) goto BNT_BAILOUT; 
 
    // read explain text into buffer. Counter names will be stored as 
 
    // a MULTI_SZ string in the format of "###" "Text..." 
 
    dwBufferSize = dwHelpSize; 
    lWin32Status = RegQueryValueEx ( 
        HKEY_PERFORMANCE_DATA, 
        HelpNameBuffer, 
        RESERVED, 
        &dwValueType, 
        (LPVOID)lpHelpText, 
        &dwBufferSize); 
 
    if (lWin32Status != ERROR_SUCCESS) goto BNT_BAILOUT; 
 
    // load counter array items, by locating each text string 
    // in the returned buffer and loading the 
    // address of it in the corresponding pointer array element. 
 
    for (lpThisName = lpCounterNames; 
 
         *lpThisName; 
         lpThisName += (lstrlen(lpThisName)+1) ) { 
 
        // first string should be an integer 
        //       (in decimal digit characters) 
        // so translate to an integer for use in array 
        //       element identification 
 
        bStatus = StringToInt (lpThisName, &dwThisCounter); 
 
        if (!bStatus) { 
            // error is in GetLastError 
            goto BNT_BAILOUT;  // bad entry 
        } 
 
        // point to corresponding counter name which follows the id number 
        // string. 
 
        lpThisName += (lstrlen(lpThisName)+1); 
 
 
        // and load array element with pointer to string 
 
        lpCounterId[dwThisCounter] = lpThisName; 
 
    } 
 
    // repeat the above for the explain text strings 
 
    for (lpThisName = lpHelpText; 
         *lpThisName; 
         lpThisName += (lstrlen(lpThisName)+1) ) { 
 
        // first string should be an integer (in decimal unicode digits) 
 
        bStatus = StringToInt (lpThisName, &dwThisCounter); 
 
        if (!bStatus) { 
            // error is in GetLastError 
            goto BNT_BAILOUT;  // bad entry 
 
        } 
 
        // point to corresponding counter name 
 
        lpThisName += (lstrlen(lpThisName)+1); 
 
        // and load array element; 
 
        lpCounterId[dwThisCounter] = lpThisName; 
 
    } 
 
    // if the last item arugment was used, 
    // then load the last ID value in it 
 
    if (pdwLastItem) *pdwLastItem = dwLastId; 
 
 
    // close the registry keys 
 
    RegCloseKey (hKeyValue); 
    RegCloseKey (HKEY_PERFORMANCE_DATA); 
 
 
    // exit returning the pointer to the buffer 
 
    return lpReturnValue; 
 
BNT_BAILOUT: 
    if (lWin32Status != ERROR_SUCCESS) { 
        // if lWin32Status has error, then set last error value to it, 
        // otherwise assume that last error already has value in it 
        SetLastError (lWin32Status); 
    } 
 
    if (lpReturnValue) { 
        free ((LPVOID)lpReturnValue); 
    } 
 
    if (bClosePerformanceKey == TRUE) RegCloseKey (HKEY_PERFORMANCE_DATA); 
    if (hKeyValue) RegCloseKey (hKeyValue); 
 
    return NULL; 
} // BuildNameTable 


  
  
/*++ 
GetSystemPerfData 
 
    Allocates data buffer as required and queries performance data 
    specified in pValue from registry. 
 
Arguments 
 
    hKeySystem 
        Handle to performance data in registry 
 
 
    pValue 
        Value string to return from registry 
 
    pPerfData 
        address of pointer to allocated perf data block that is 
        filled in by call to RegQueryValue. 
 
Return Value 
 
    ERROR_SUCCESS if data returned 
    WIN 32 ERROR if error encountered 
--*/ 
DWORD 
GetSystemPerfData ( 
    IN HKEY hKeySystem, 
    IN LPCTSTR   pValue, 
    IN PPERF_DATA_BLOCK *pPerfData 
) 
{  // GetSystemPerfData 
    LONG     lError ;   // Win 32 Error returned by fn. calls 
    DWORD    Size;      // size of data buffer passed to fn. call 
    DWORD    Type;      // type of data buffer returned by fn. call 
 
 
    // allocate initial buffer if one is not passed into this routine 
 
    if (*pPerfData == NULL) { 
        *pPerfData = malloc (INITIAL_SIZE); 
        if (*pPerfData == NULL) { 
            return ERROR_OUTOFMEMORY; 
        } 
    } 
 
    while (TRUE) { 
        Size = _msize (*pPerfData); // query the size of the data buffer 
 
        lError = RegQueryValueEx (  // get performance data from reg. 
            hKeySystem, 
            pValue, 
            RESERVED, 
            &Type, 
            (LPBYTE)*pPerfData, 
 
            &Size) ; 
 
        // check for success and valid perf data block signature 
 
        if ((!lError) && 
            (Size > 0) && 
            (*pPerfData)->Signature[0] == (WCHAR)'P' && 
            (*pPerfData)->Signature[1] == (WCHAR)'E' && 
            (*pPerfData)->Signature[2] == (WCHAR)'R' && 
            (*pPerfData)->Signature[3] == (WCHAR)'F' ) { 
 
            return (ERROR_SUCCESS) ; 
        } 
 
        // if buffer is not big enough, reallocate and try again 
 
 
        if (lError == ERROR_MORE_DATA) { 
            *pPerfData = realloc ( 
                *pPerfData, 
                _msize (*pPerfData) + 
                EXTEND_SIZE) ; 
 
            if (!*pPerfData) { 
                return (lError) ; 
            } 
        } else { 
            return (lError) ; 
        } 
    } 
}  // GetSystemPerfData 
 

   
   
/*++ 
 
Pointer Functions 
 
    These functions are used to walk down the various structures 
    in the perf data block returned by the call to RegQueryValueEx 
 
 
--*/ 
PPERF_OBJECT_TYPE 
FirstObject ( 
    PPERF_DATA_BLOCK pPerfData 
) 
{ 
    return ((PPERF_OBJECT_TYPE) ((PBYTE) pPerfData + pPerfData->HeaderLength)) ; 
} 
 
PPERF_OBJECT_TYPE 
NextObject ( 
    PPERF_OBJECT_TYPE pObject 
) 
{  // NextObject 
    return ((PPERF_OBJECT_TYPE) ((PBYTE) pObject + pObject->TotalByteLength)) ; 
}  // NextObject 
 
PERF_COUNTER_DEFINITION * 
FirstCounter( 
    PERF_OBJECT_TYPE *pObjectDef 
) 
{ 
    return (PERF_COUNTER_DEFINITION *) 
 
               ((PCHAR) pObjectDef + pObjectDef->HeaderLength); 
} 
 
PERF_COUNTER_DEFINITION * 
NextCounter( 
    PERF_COUNTER_DEFINITION *pCounterDef 
) 
{ 
    return (PERF_COUNTER_DEFINITION *) 
               ((PCHAR) pCounterDef + pCounterDef->ByteLength); 
} 
 
PERF_INSTANCE_DEFINITION * 
FirstInstance ( 
    PERF_OBJECT_TYPE    *pObject 
) 
{ 
    return (PERF_INSTANCE_DEFINITION *) 
        ((PBYTE) pObject + pObject->DefinitionLength); 
 
} 
 
PERF_INSTANCE_DEFINITION * 
NextInstance ( 
    PERF_INSTANCE_DEFINITION *pInstance 
) 
{ 
    // next instance is after 
    //    this instance + this instances counter data 
    PERF_COUNTER_BLOCK  *pCtrBlk; 
 
    pCtrBlk = (PERF_COUNTER_BLOCK *) 
        ((PBYTE)pInstance + pInstance->ByteLength); 
 
    return (PERF_INSTANCE_DEFINITION *) 
        ((PBYTE)pInstance + pInstance->ByteLength + pCtrBlk->ByteLength); 
 
} 





/*
 * Given a name, find the index in the string table. The
 * index is returned in string form and DWORD form.
 */
BOOL getStringIndex (LPTSTR *lppCounterNames,	// the string table
					 DWORD dwLastCounter,		// entries in string table
					 LPCTSTR objectName,		// name for which index is required
					 LPTSTR stringIndex,		// return index in string form
					 DWORD *dwIndex)			// return index in DWORD form
{ 
    DWORD   dwElem; 
 
	for (dwElem = 0; dwElem <= dwLastCounter; dwElem++) { 
        // compare counter name to command line 
		if (lppCounterNames[dwElem]) { 
			if ((lstrcmpi(lppCounterNames[dwElem], objectName)) == 0) { 
				// match found 
                // make element Number a string and exit 
				if (stringIndex != NULL) {
					_stprintf ((LPTSTR)stringIndex, TEXT("%d"), dwElem);
				}
				if (dwIndex != NULL) {
					*dwIndex = dwElem;
				}
                return TRUE;
			} // endif command line matches string 
		} // endif lppCounterNames[dwElem] not equal NULL 
	} // end for loop that searches table 
	return FALSE;
} // ProcessCommandLine 




/*
 * A large integer subtraction routine with assumption that a > b
 */
void largeSubtract(const LARGE_INTEGER *a,	// To be subtracted from.
				   const LARGE_INTEGER *b,	// To subtract
				   LARGE_INTEGER *result)	// Result of subtraction
{
	if (a->LowPart >= b->LowPart) {
		result->LowPart = a->LowPart - b->LowPart;
		result->HighPart = a->HighPart - b->HighPart;
	}
	else {
		result->LowPart = (0xffffffff - b->LowPart);
		result->LowPart += 1 + a->LowPart;
		result->HighPart = a->HighPart - b->HighPart - 1;
	}
}




/*
 * Get the "% Total Processor Time" average over time since
 * the last call. The first call always returns 0. Return -1
 * on error.
 */
int getLoad(LPCTSTR *lppCounterText,	// String table
			DWORD dwLastCounter,		// entries in string table
			LPCTSTR stringIndex,		// index of object in string table as string
			DWORD dwIndex,				// index of object in string table as DWORD
			LPCTSTR objectName,			// name of object for which counter is required
			LPCTSTR counterName)		// name of required counter
{
	static LARGE_INTEGER last100nsTimer = { 0, 0 };
	static LARGE_INTEGER lastRateCount = { 0, 0 };

	int load;
	PPERF_DATA_BLOCK pDataBlock;

	pDataBlock = NULL;
	if (GetSystemPerfData(HKEY_PERFORMANCE_DATA, stringIndex, &pDataBlock) == ERROR_SUCCESS) {
		PPERF_OBJECT_TYPE pObjType;
		PPERF_COUNTER_DEFINITION counterDef;
		DWORD i;

		// Iterate through the objects received and find the one we want.
		for (i = 0, pObjType = FirstObject(pDataBlock);
			 i < pDataBlock->NumObjectTypes;
			 i++, pObjType = NextObject(pObjType)) {
			if (pObjType->ObjectNameTitleIndex == dwIndex) {
				break;
			}
		}

		if (i < pDataBlock->NumObjectTypes) { // Object found
			// Iterate through the counters for the object and attempt
			// to locate the one we want.
			for (i = 0, counterDef = FirstCounter(pObjType);
				 i < pObjType->NumCounters;
				 i++, counterDef = NextCounter(counterDef)) {
				if (counterDef->CounterType == PERF_100NSEC_TIMER_INV &&
					lstrcmpi(lppCounterText[counterDef->CounterNameTitleIndex], counterName) == 0) {
					break;
				}
			}
			if (i < pObjType->NumCounters) { // Counter found
				LARGE_INTEGER *lint, deltaTime, deltaRateCount;
				PPERF_COUNTER_BLOCK pCounterBlock;
				
				// Calculate the load.
				pCounterBlock = (PPERF_COUNTER_BLOCK) ((char*) pObjType + pObjType->DefinitionLength);
				lint = (LARGE_INTEGER*) ((char*) pCounterBlock + counterDef->CounterOffset);
				if (last100nsTimer.LowPart == 0 && last100nsTimer.HighPart == 0) {
					load = 0;
				}
				else {
					largeSubtract(&pDataBlock->PerfTime100nSec, &last100nsTimer, &deltaTime);
					largeSubtract(lint, &lastRateCount, &deltaRateCount);
					// We assume here that the high part of the deltas will always be
					// zero as we will not have a very long polling interval.
					load = 100 - (int) ((double) deltaRateCount.LowPart / deltaTime.LowPart * 100);
					if (load < 0) {
						// For very short polling intervals, sometimes we can get
						// negative load. This is perhaps cause by synchronisation
						// problems in NT's kernel.
						load = 0;
					}
				}
				last100nsTimer = pDataBlock->PerfTime100nSec;
				lastRateCount = *lint;
			}
			else { // Counter not found
				load = -1;
			}
		}
		else { // Object not found
			load = -1;
		}
	
		free(pDataBlock);
		return load;

	}
	else { // Can't seemed to get performance data
		return -1;
	}
}



/*
 * Set the load variable using the mutex for mutual exclusion.
 */
void setGlobalLoadValue(int value)
{
	WaitForSingleObject(Mutex, INFINITE);
	Load[CurLoadIndex] = value;
	CurLoadIndex = (CurLoadIndex + 1) % SAMPLE;
	ReleaseMutex(Mutex);
}



/*
 * Get the load variable using the mutex for mutual exclusion.
 */
int getGlobalLoadValue()
{
	int i, sum = 0;
	WaitForSingleObject(Mutex, INFINITE);
	for (i = 0; i < SAMPLE; i++) {
		if (Load[i] >= 0) {
			sum += Load[i];
		}
		else {
			ReleaseMutex(Mutex);
			return -1;
		}
	}
	ReleaseMutex(Mutex);
	return sum / SAMPLE;
}



/*
 * This will be run in a process to repeatedly get the load information
 * from the system and stores it in the global load variable.
 */
DWORD WINAPI repeatedlyGetLoad(LPVOID ignored)
{
	LPTSTR *lppCounterText = NULL;
	DWORD dwLastElement = 0;
	WCHAR stringIndex[20];
	DWORD dwIndex;
	int currentLoad;

	// Build the string table.
    lppCounterText = BuildNameTable (HKEY_LOCAL_MACHINE, DefaultLangId, &dwLastElement); 
	if (lppCounterText == NULL) {
		setGlobalLoadValue(-1);
		return 255;
	}

	// Get the index of the object into the string table.
	if (!getStringIndex(lppCounterText, dwLastElement, ObjectName, stringIndex, &dwIndex)) {
		setGlobalLoadValue(-1);
		return 255;
	}

	// Initialise the load variable by polling twice in a second.
	if (getLoad(lppCounterText, dwLastElement, stringIndex, dwIndex, ObjectName, CounterName) < 0) {
		setGlobalLoadValue(-1);
		return 255;
	}
	Sleep(1000);
	currentLoad = getLoad(lppCounterText, dwLastElement, stringIndex, dwIndex, ObjectName, CounterName);
	if (currentLoad < 0) {
		setGlobalLoadValue(-1);
		return 255;
	}

	// Loop through every 5 seconds.
	while (TRUE) {
		Sleep(SLEEP_TIME);
		currentLoad = getLoad(lppCounterText, dwLastElement, stringIndex, dwIndex, ObjectName, CounterName);
		setGlobalLoadValue(currentLoad);
		if (currentLoad < 0) {
			return 255;
		}
	}
}




int main(int argc, char *argv[])
{
	HANDLE in, out;		// Standard input and output handles
	char inMsg;
	DWORD bytesRead;
	HANDLE thread;
	DWORD threadId;
	char outMsg[4];
	DWORD bytesWritten;
	int i;

	for (i = 0; i < SAMPLE; i++) {
		Load[i] = 0;
	}
	Mutex = CreateMutex(NULL, FALSE, NULL);

	in = GetStdHandle(STD_INPUT_HANDLE);
	out = GetStdHandle(STD_OUTPUT_HANDLE);
	SetConsoleMode(in, ENABLE_PROCESSED_INPUT);
	SetConsoleMode(out, 0);

 	thread = CreateThread(NULL, 0, repeatedlyGetLoad, NULL, 0, &threadId);
	if (thread == NULL) {
		return -1;
	}

	while (ReadFile(in, &inMsg, sizeof(inMsg), &bytesRead, NULL)) {
		if (inMsg == '\0') {
			int currentLoad = getGlobalLoadValue();
			if (currentLoad < 0) { // Something went wrong
				return -1;
			}
			outMsg[0] = (char) ((currentLoad >> 24) & 0xff);
			outMsg[1] = (char) ((currentLoad >> 16) & 0xff);
			outMsg[2] = (char) ((currentLoad >> 8) & 0xff);
			outMsg[3] = (char) (currentLoad & 0xff);
			if (!WriteFile(out, outMsg, 4, &bytesWritten, NULL)) {
				return -1;
			}
		}
		else { // Unknown message
			return -1;
		}
	}

	return 0;
}
